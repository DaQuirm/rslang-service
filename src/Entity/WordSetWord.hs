{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Entity.WordSetWord where

import Prelude hiding (Word, id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), autoPrimary)
import Data.Aeson (ToJSON, FromJSON)

import IDAesonInstances
import Entity.Word (Word)
import Entity.WordSet (WordSet)

data WordSetWord = WordSetWord
  { wordset :: ID WordSet
  , word    :: ID Word
  } deriving (Show, Generic)

instance SqlRow WordSetWord

wordSetWordsTable :: Table WordSetWord
wordSetWordsTable = table "wordset_words" []
