{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Entity.User where

import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Database.Selda (SqlRow, Table, table, Attr((:-)), primary)

data User = User
  { id :: Text
  } deriving (Show, Generic)

instance SqlRow User

instance ToJSON User
instance FromJSON User

usersTable :: Table User
usersTable = table "users" [#id :- primary]
