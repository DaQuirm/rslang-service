module IDAesonInstances where

import Database.Selda (ID, fromId, toId)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))

instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON = (toId <$>) . parseJSON