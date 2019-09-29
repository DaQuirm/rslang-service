{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.Word where

import Prelude hiding (Word, id)
import Data.Text (Text)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (ID, RowID, query, limit, select, restrict, (!), (.==), literal, toId, text, insertWithPK, def)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import PostgresConnectionSettings (connectionSettings)
import Entity.Word (Word, WordW, wordsTable, fromWordW)

type WordAPI
  = "words" :>
    (    Capture "id" Int :> Get '[JSON] Word
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [Word]
    :<|> ReqBody '[JSON] WordW :> Post '[JSON] (ID Word)
    )

wordAPI
  =    getWord
  :<|> getWords
  :<|> addWord
  where
    getWord :: Int -> Handler Word
    getWord wordId =
      withPostgreSQL connectionSettings $ do
        fmap head $ query $ limit 0 1 $ do
          word <- select wordsTable
          restrict (word ! #id .== (literal $ toId wordId))
          return word

    getWords :: Text -> Handler [Word]
    getWords userId = do
      withPostgreSQL connectionSettings $ do
        query $ do
          word <- select wordsTable
          restrict (word ! #added_by .== text userId)
          return word

    addWord :: WordW -> Handler (ID Word)
    addWord word = do
      withPostgreSQL connectionSettings $ do
        insertWithPK wordsTable [fromWordW def word]