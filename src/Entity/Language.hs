{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Entity.Language where

import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), unique, foreignKey)
import Data.Aeson (ToJSON, FromJSON)

data Language = Language
  { code     :: Text
  , name     :: Text
  } deriving (Show, Generic)

instance SqlRow Language
instance ToJSON Language
instance FromJSON Language

languagesTable :: Table Language
languagesTable = table "languages" [ #code :- unique ]