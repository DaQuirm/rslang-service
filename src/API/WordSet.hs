{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module API.WordSet where

import Prelude hiding (id)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty((:|)), groupBy, toList)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (query, select, restrict, insertWithPK, insert_, def, (!), (.==), literal, toId, second, (:*:)((:*:)))
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import Entity.Word (wordsTable)
import Entity.WordSet (WordSet(WordSet), wordSetsTable, created_by, id)
import Entity.WordSetWord (WordSetWord(WordSetWord), wordSetWordsTable)
import Payload.WordSetWords (WordSetWordsR(WordSetWordsR), WordSetWordsW(WordSetWordsW))

type WordSetAPI
  = "wordsets" :>
    (    Capture "id" Int :> Get '[JSON] (Maybe WordSetWordsR)
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [WordSetWordsR]
    :<|> ReqBody '[JSON] WordSetWordsW :> Post '[JSON] ()
    )

wordSetAPI
  =    getWordSet
  :<|> getUserWordSets
  :<|> addWordSet
  where
    getWordSet :: Int -> Handler (Maybe WordSetWordsR)
    getWordSet wsId = do
      withPostgreSQL ("lang" `on` "localhost") $ do
        ps <- query $ do
          wordSet <- select wordSetsTable
          restrict $ wordSet ! #id .== (literal $ toId wsId)
          wordSetWord <- select wordSetWordsTable
          restrict $ wordSetWord ! #wordset .== (literal $ toId wsId)
          word <- select wordsTable
          restrict $ word ! #id .== wordSetWord ! #word
          return $ wordSet :*: word
        case ps of
          [] -> pure Nothing
          rows@((WordSet {created_by} :*: _) : _) -> do
            let words = second <$> rows
            return $ Just $ WordSetWordsR (toId wsId) created_by words

    getUserWordSets :: Text -> Handler [WordSetWordsR]
    getUserWordSets userId = do
        withPostgreSQL ("lang" `on` "localhost") $ do
          ps <- query $ do
            wordSet <- select wordSetsTable
            restrict $ wordSet ! #created_by .== (literal $ userId)
            wordSetWord <- select wordSetWordsTable
            restrict $ wordSetWord ! #wordset .== (wordSet ! #id)
            word <- select wordsTable
            restrict $ word ! #id .== wordSetWord ! #word
            return $ wordSet :*: word
          case ps of
            [] -> pure []
            rows -> do
              let groupedRows = groupBy (\(wsA :*: _) (wsB :*: _) -> (id wsA) == (id wsB)) rows
                  toWordSetWords pairs@((ws :*: _) :| _) =
                    let wsId = id ws
                        createdBy = created_by ws
                        words = toList (second <$> pairs)
                     in WordSetWordsR wsId createdBy words
              pure $ toWordSetWords <$> groupedRows


    addWordSet :: WordSetWordsW -> Handler ()
    addWordSet (WordSetWordsW createdBy wordIds) = do
      withPostgreSQL ("lang" `on` "localhost") $ do
        wordSetId <- insertWithPK wordSetsTable [WordSet def createdBy]
        let mkWordSetWordEntry wordId = WordSetWord wordSetId wordId
        insert_ wordSetWordsTable (mkWordSetWordEntry <$> wordIds)
