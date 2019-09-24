{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.Translation where

import Data.Text (Text)

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Capture, Handler, (:<|>)(..))
import RequiredQueryParam (RequiredQueryParam)

import Database.Selda (ID, query, limit, select, restrict, (!), (.==), literal, toId, text, insertWithPK, def)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import PostgresConnectionSettings (connectionSettings)
import Entity.Translation (Translation, TranslationW, translationsTable, fromTranslationW)

type TranslationAPI
  = "translations" :>
    (    Capture "id" Int :> Get '[JSON] Translation
    :<|> RequiredQueryParam "userId" Text :> Get '[JSON] [Translation]
    :<|> ReqBody '[JSON] TranslationW :> Post '[JSON] (ID Translation)
    )

translationAPI
  =    getTranslation
  :<|> getTranslations
  :<|> addTranslation
  where
    getTranslation :: Int -> Handler Translation
    getTranslation translationId = do
      withPostgreSQL connectionSettings $ do
        fmap head $ query $ limit 0 1 $ do
          translation <- select translationsTable
          restrict (translation ! #id .== (literal $ toId translationId))
          return translation

    getTranslations :: Text -> Handler [Translation]
    getTranslations userId = do
      withPostgreSQL connectionSettings $ do
        query $ do
          translation <- select translationsTable
          restrict (translation ! #added_by .== text userId)
          return translation

    addTranslation :: TranslationW -> Handler (ID Translation)
    addTranslation translation = do
      withPostgreSQL connectionSettings $ do
        insertWithPK translationsTable [fromTranslationW def translation]

