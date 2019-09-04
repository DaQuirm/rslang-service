{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Entity.Translation where

import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), autoPrimary)
import Data.Aeson (ToJSON, FromJSON)

import IDAesonInstances

data Translation = Translation
  { id       :: ID Translation
  , word     :: ID Word
  , text     :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance SqlRow Translation

data TranslationW = TranslationW
  { word     :: ID Word
  , text     :: Text
  , language :: Text
  , added_by :: Text
  } deriving (Show, Generic)

instance ToJSON Translation
instance FromJSON TranslationW

translationsTable :: Table Translation
translationsTable = table "translations" [#id :- autoPrimary]

fromTranslationW :: ID Translation -> TranslationW -> Translation
fromTranslationW trId TranslationW { word, text, language, added_by } =
  Translation trId word text language added_by
