{-# LANGUAGE NamedFieldPuns #-}

module ServiceState where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.Selda.Backend (SeldaConnection)
import Database.Selda.PostgreSQL (PG)
import Data.Pool (Pool)

type ServiceStats = Map Text Int

data ServiceState  = ServiceState
  { stats          :: ServiceStats
  , connectionPool :: Pool (SeldaConnection PG)
  }

tick :: Text -> ServiceState -> (Int, ServiceState)
tick key state@ServiceState{ stats } =
  let requestCount = fromMaybe 0 $ Map.lookup key stats
      newCount = requestCount + 1
      newStats = Map.insert key newCount stats
   in (newCount, state { stats = newStats })