{-# LANGUAGE OverloadedStrings #-}

module API.Doc.Word where

import Prelude hiding (Word)

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Proxy (Proxy(Proxy))

import Database.Selda (toId)
import Data.Aeson (toJSON)

import Data.Swagger (Swagger, ToSchema(declareNamedSchema), genericDeclareNamedSchema, defaultSchemaOptions, info, license, description, schema, example, title, version, URL(URL), url)
import Servant.Swagger (toSwagger)

import API.Doc.ToSchema ()
import Entity.Word (Word(Word), WordW(WordW))
import API.Word (WordAPI)

instance ToSchema Word where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A single word"
    & mapped.schema.example ?~ toJSON (Word (toId 0) "vatten" "swe" "user37")

instance ToSchema WordW where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A single word without the id"
    & mapped.schema.example ?~ toJSON (WordW "vatten" "swe" "user37")

wordAPISwagger :: Swagger
wordAPISwagger = toSwagger (Proxy :: Proxy WordAPI)
  & info.title   .~ "Word API"
  & info.version .~ "0.0.1"
  & info.description ?~ "This is an API for single words"