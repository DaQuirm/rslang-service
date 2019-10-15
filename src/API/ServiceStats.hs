{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.ServiceStats where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.MVar (readMVar)
import Servant (type (:>), Get, JSON, Handler)

import ServiceState (ServiceStats, stats, tick)
import App (AppT, API, modifyState)

type ServiceStatsAPI = "stats" :> Get '[JSON] ServiceStats

serviceStatsAPI :: API ServiceStatsAPI
serviceStatsAPI
  = getStats
      where
        getStats :: AppT Handler ServiceStats
        getStats = do
          modifyState $ tick "getStats"
          stateVar <- ask
          liftIO $ stats <$> readMVar stateVar