{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module App where

import Data.Tuple (swap)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar)
import Data.Pool (withResource)
import Servant (Handler, ServerT)
import Database.Selda.Backend (MonadSelda(Backend, withConnection))
import Database.Selda.PostgreSQL (PG)

import ServiceState (ServiceState(ServiceState), connectionPool)

type AppT m = ReaderT (MVar ServiceState) m

instance (MonadIO m, MonadBaseControl IO m) => MonadSelda (AppT m) where
  type Backend (AppT m) = PG
  withConnection m = do
    stateVar <- ask
    ServiceState{ connectionPool } <- liftIO $ readMVar stateVar
    withResource connectionPool m

type API api = ServerT api (AppT Handler)

modifyState :: (ServiceState -> (a, ServiceState)) -> AppT Handler a
modifyState modification = do
  stateVar <- ask
  liftIO $ modifyMVar stateVar $ pure . swap . modification


