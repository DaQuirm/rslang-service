{-# LANGUAGE OverloadedStrings #-}

module API.Doc.User where

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Proxy (Proxy(Proxy))

import Data.Aeson (toJSON)

import Data.Swagger (Swagger, ToSchema(declareNamedSchema), genericDeclareNamedSchema, defaultSchemaOptions, info, description, schema, example, title, version)
import Servant.Swagger (toSwagger)

import API.Doc.ToSchema ()
import Entity.User (User(User))
import API.User (UserAPI)

instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A user (unique name)"
    & mapped.schema.example ?~ toJSON (User "espectro")

userAPISwagger :: Swagger
userAPISwagger = toSwagger (Proxy :: Proxy UserAPI)
  & info.title   .~ "User API"
  & info.version .~ "0.0.1"
  & info.description ?~ "This is an API for users"
