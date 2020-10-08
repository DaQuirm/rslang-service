{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.Word where

import Prelude hiding (Word, id)
import Data.Text (Text)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (ID, query, limit, select, restrict, (!), (.==), literal, toId, text, insertWithPK, def)

import ServiceState (tick)
import App (API, AppT, modifyState)
import Entity.Word (Word, WordW, wordsTable, fromWordW)

type WordAPI
  = "words" :>
    (    Capture "id" Int :> Get '[JSON] Word
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [Word]
    :<|> ReqBody '[JSON] WordW :> Post '[JSON] (ID Word)
    )

wordAPI :: API WordAPI
wordAPI
  =    getWord
  :<|> getWords
  :<|> addWord
  where
    getWord :: Int -> AppT Handler Word
    getWord wordId = do
      modifyState $ tick "getWord"
      fmap head $ query $ limit 0 1 $ do
        word <- select wordsTable
        restrict (word ! #id .== (literal $ toId wordId))
        return word

    getWords :: Text -> AppT Handler [Word]
    getWords userId = do
      modifyState $ tick "getWords"
      query $ do
        word <- select wordsTable
        restrict (word ! #added_by .== text userId)
        return word

    addWord :: WordW -> AppT Handler (ID Word)
    addWord word = do
      modifyState $ tick "addWord"
      insertWithPK wordsTable [fromWordW def word]
