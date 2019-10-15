{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Network.Wai.Handler.Warp (run)

import Servant (type (:<|>), Proxy(Proxy), (:<|>)(..), hoistServer, Handler)
import Servant.Server (serve)

import API.User (UserAPI, userAPI)
import API.Translation (TranslationAPI, translationAPI)
import API.Word (WordAPI, wordAPI)
import API.WordSet (WordSetAPI, wordSetAPI)
import API.DBSchema (DBSchemaAPI, dbSchemaAPI)
import API.Swagger (SwaggerAPI, swaggerAPI)
import API.ServiceStats (ServiceStatsAPI, serviceStatsAPI)

import ServiceState (ServiceState(ServiceState), connectionPool, stats)
import App (AppT)

import Control.Monad.Trans.Reader (runReaderT)
import Data.Map.Strict (empty)
import Control.Concurrent.MVar (MVar, newMVar)
import Database.Selda.PostgreSQL (pgOpen, seldaClose)
import Data.Pool (createPool)
import PostgresConnectionSettings (connectionSettings)

type ServiceAPI
  =    UserAPI
  :<|> WordAPI
  :<|> WordSetAPI
  :<|> TranslationAPI
  :<|> DBSchemaAPI
  :<|> SwaggerAPI
  :<|> ServiceStatsAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

appTToHandler :: (MVar ServiceState) -> AppT Handler a -> Handler a
appTToHandler stateVar value = runReaderT value stateVar

main :: IO ()
main = do
  connectionPool <- createPool (pgOpen connectionSettings) seldaClose 1 30 16
  stateVar <- newMVar $ ServiceState { connectionPool, stats = empty }
  let server
        =    userAPI
        :<|> wordAPI
        :<|> wordSetAPI
        :<|> translationAPI
        :<|> dbSchemaAPI
        :<|> pure swaggerAPI
        :<|> serviceStatsAPI
      app = serve serviceAPI (hoistServer serviceAPI (appTToHandler stateVar) server)
  run 3000 app
