{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Entity.WordSet where

import Prelude hiding (Word, id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), autoPrimary)
import Data.Aeson (ToJSON, FromJSON)

import IDAesonInstances
import Entity.Word (Word)

data WordSet = WordSet
  { id         :: ID WordSet
  , created_by :: Text
  } deriving (Show, Generic)

instance SqlRow WordSet

wordSetsTable :: Table WordSet
wordSetsTable = table "wordsets" [#id :- autoPrimary]
