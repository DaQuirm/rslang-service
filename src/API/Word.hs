{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}

module API.Word where

import Prelude hiding (Word)
import Data.Text (Text)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import Servant.Docs (ToCapture(toCapture), DocCapture(DocCapture), ToParam(toParam), DocQueryParam(DocQueryParam), ParamKind(Normal), ToSample(toSamples), singleSample)
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (ID, query, limit, select, restrict, (!), (.==), literal, toId, text, insertWithPK, def)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import Entity.Word (Word(Word), WordW(WordW), wordsTable, fromWordW)

type WordAPI
  = "words" :>
    (    Capture "id" Int :> Get '[JSON] Word
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [Word]
    :<|> ReqBody '[JSON] WordW :> Post '[JSON] (ID Word)
    )

instance ToCapture (Capture "id" Int) where
  toCapture _
    = DocCapture
      "id"
      "word id (integer)"

instance ToParam (RequiredQueryParam "userId" Text) where
  toParam _ =
    DocQueryParam "userId"
                  ["anonymous", "lesnitsky", "..."]
                  "Text id of the user"
                  Normal

instance ToSample Word where
  toSamples _ = singleSample $ Word (toId 2) "vatten" "swe" "anonymous"

instance ToSample WordW where
  toSamples _ = singleSample $ WordW "vatten" "swe" "anonymous"

instance ToSample (ID Word) where
  toSamples _ = singleSample $ toId 2

wordAPI
  =    getWord
  :<|> getWords
  :<|> addWord
  where
    getWord :: Int -> Handler Word
    getWord wordId =
      withPostgreSQL ("lang" `on` "localhost") $ do
        fmap head $ query $ limit 0 1 $ do
          word <- select wordsTable
          restrict (word ! #id .== (literal $ toId wordId))
          return word

    getWords :: Text -> Handler [Word]
    getWords userId = do
      withPostgreSQL ("lang" `on` "localhost") $ do
        query $ do
          word <- select wordsTable
          restrict (word ! #added_by .== text userId)
          return word

    addWord :: WordW -> Handler (ID Word)
    addWord word = do
      withPostgreSQL ("lang" `on` "localhost") $ do
        insertWithPK wordsTable [fromWordW def word]