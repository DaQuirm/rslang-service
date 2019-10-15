{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Swagger where

import Servant (type (:>), JSON, Get, Handler, (:<|>)(..))
import Data.Swagger (Swagger)

import API.Doc.Translation (translationAPISwagger)
import API.Doc.User (userAPISwagger)
import API.Doc.Word (wordAPISwagger)
import API.Doc.WordSet (wordSetAPISwagger)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

swaggerAPI
  =  translationAPISwagger
  <> userAPISwagger
  <> wordAPISwagger
  <> wordSetAPISwagger
