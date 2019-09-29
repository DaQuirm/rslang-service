{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Entity.Translation where

import Prelude hiding (Word, id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), autoPrimary, foreignKey)
import Data.Aeson (ToJSON, FromJSON)

import Entity.User (usersTable)
import Entity.Word (Word, wordsTable)

data Translation = Translation
  { id       :: ID Translation
  , word     :: ID Word
  , text     :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance SqlRow Translation
instance ToJSON Translation

data TranslationW = TranslationW
  { word     :: ID Word
  , text     :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance FromJSON TranslationW

translationsTable :: Table Translation
translationsTable = table "translations"
  [ #id       :- autoPrimary
  , #word     :- foreignKey wordsTable #id
  , #added_by :- foreignKey usersTable #id
  ]

fromTranslationW :: ID Translation -> TranslationW -> Translation
fromTranslationW trId TranslationW { word, text, language, added_by } =
  Translation trId word text language added_by
