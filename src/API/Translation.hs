{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.Translation where

import Data.Text (Text)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (ID, query, limit, select, restrict, (!), (.==), literal, toId, text, insertWithPK, def)

import ServiceState (tick)
import App (API, AppT, modifyState)
import Entity.Translation (Translation, TranslationW, translationsTable, fromTranslationW)

type TranslationAPI
  = "translations" :>
    (    Capture "id" Int :> Get '[JSON] Translation
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [Translation]
    :<|> ReqBody '[JSON] TranslationW :> Post '[JSON] (ID Translation)
    )

translationAPI :: API TranslationAPI
translationAPI
  =    getTranslation
  :<|> getTranslations
  :<|> addTranslation
  where
    getTranslation :: Int -> AppT Handler Translation
    getTranslation translationId = do
      modifyState $ tick "getTranslation"
      fmap head $ query $ limit 0 1 $ do
        translation <- select translationsTable
        restrict (translation ! #id .== (literal $ toId translationId))
        return translation

    getTranslations :: Text -> AppT Handler [Translation]
    getTranslations userId = do
      modifyState $ tick "getTranslations"
      query $ do
        translation <- select translationsTable
        restrict (translation ! #added_by .== text userId)
        return translation

    addTranslation :: TranslationW -> AppT Handler (ID Translation)
    addTranslation translation = do
      modifyState $ tick "addTranslation"
      insertWithPK translationsTable [fromTranslationW def translation]

