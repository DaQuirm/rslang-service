{-# LANGUAGE OverloadedStrings #-}

module API.Doc.Translation where

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Proxy (Proxy(Proxy))

import Database.Selda (toId)
import Data.Aeson (toJSON)

import Data.Swagger (Swagger, ToSchema(declareNamedSchema), genericDeclareNamedSchema, defaultSchemaOptions, info, description, schema, example, title, version)
import Servant.Swagger (toSwagger)

import API.Doc.ToSchema ()
import Entity.Translation (Translation(Translation), TranslationW(TranslationW))
import API.Translation (TranslationAPI)

instance ToSchema Translation where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A word translation"
    & mapped.schema.example ?~ toJSON (Translation (toId 0) (toId 1) "water" "eng" "user37")

instance ToSchema TranslationW where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A word translation without the id"
    & mapped.schema.example ?~ toJSON (TranslationW (toId 1) "water" "eng" "user37")

translationAPISwagger :: Swagger
translationAPISwagger = toSwagger (Proxy :: Proxy TranslationAPI)
  & info.title   .~ "Translation API"
  & info.version .~ "0.0.1"
  & info.description ?~ "This is an API for word translations"
