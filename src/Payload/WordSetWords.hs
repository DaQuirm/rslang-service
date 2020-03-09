{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Payload.WordSetWords where

import Prelude hiding (Word)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.Selda (ID, SqlRow)

import Entity.Word (Word)
import Entity.WordSet (WordSet)

data WordSetWordsR = WordSetWordsR
  { id          :: ID WordSet
  , created_by  :: Text
  , words       :: [Word]
  } deriving (Show, Generic)

instance ToJSON WordSetWordsR

data WordSetWordsW = WordSetWordsW
  { created_by  :: Text
  , words       :: [ID Word]
  } deriving (Show, Generic)

instance ToJSON WordSetWordsW
instance FromJSON WordSetWordsW