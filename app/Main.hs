{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)

import Servant (type (:>), type (:<|>), Proxy(Proxy), (:<|>)(..))
import Servant.Server (serve)

import API.User (UserAPI, userAPI)
import API.Translation (TranslationAPI, translationAPI)
import API.Word (WordAPI, wordAPI)
import API.WordSet (WordSetAPI, wordSetAPI)
import API.DBSchema (DBSchemaAPI, dbSchemaAPI)
import API.Swagger (SwaggerAPI, swaggerAPI)

type ServiceAPI
  =    UserAPI
  :<|> WordAPI
  :<|> WordSetAPI
  :<|> TranslationAPI
  :<|> DBSchemaAPI
  :<|> SwaggerAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

main :: IO ()
main = do
  let server
        =    userAPI
        :<|> wordAPI
        :<|> wordSetAPI
        :<|> translationAPI
        :<|> dbSchemaAPI
        :<|> pure swaggerAPI

      app = serve serviceAPI server
  run 3000 app
