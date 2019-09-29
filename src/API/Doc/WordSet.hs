{-# LANGUAGE OverloadedStrings #-}

module API.Doc.WordSet where

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Proxy (Proxy(Proxy))

import Database.Selda (toId)
import Data.Aeson (toJSON)

import Data.Swagger (Swagger, ToSchema(declareNamedSchema), genericDeclareNamedSchema, defaultSchemaOptions, info, license, description, schema, example, title, version, URL(URL), url)
import Servant.Swagger (toSwagger)

import API.Doc.ToSchema ()
import API.Doc.Word ()
import Payload.WordSetWords (WordSetWordsR(WordSetWordsR), WordSetWordsW(WordSetWordsW))
import Entity.Word (Word(Word))
import API.WordSet (WordSetAPI)

instance ToSchema WordSetWordsR where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A word set"
    & mapped.schema.example ?~ toJSON (WordSetWordsR (toId 0) "espectro" words)
      where
        words =
          [ Word (toId 1) "vatten" "swe" "user37"
          , Word (toId 2) "Wasser" "deu" "user37"
          , Word (toId 3) "water"  "eng" "user42"
          ]

instance ToSchema WordSetWordsW where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A word set (client payload)"
    & mapped.schema.example ?~ toJSON (WordSetWordsW "espectro" (toId <$> [1, 2, 3]))

wordSetAPISwagger :: Swagger
wordSetAPISwagger = toSwagger (Proxy :: Proxy WordSetAPI)
  & info.title   .~ "WordSet API"
  & info.version .~ "0.0.1"
  & info.description ?~ "An API for word sets"