{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Entity.Word where

import Prelude hiding (Word, id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), autoPrimary, foreignKey)
import Database.Selda.JSON ()
import Data.Aeson (ToJSON, FromJSON)

import Entity.User (usersTable)
import Entity.Language (languagesTable)

data Word = Word
  { id       :: ID Word
  , string   :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance SqlRow Word
instance ToJSON Word

data WordW = WordW
  { string   :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance ToJSON WordW
instance FromJSON WordW

wordsTable :: Table Word
wordsTable = table "words"
  [ #id       :- autoPrimary
  , #language :- foreignKey languagesTable #code
  , #added_by :- foreignKey usersTable #id
  ]

fromWordW :: ID Word -> WordW -> Word
fromWordW wordId WordW { string, language, added_by } = Word wordId string language added_by