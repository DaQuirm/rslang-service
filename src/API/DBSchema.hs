{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.DBSchema where

import Data.Text (Text)

import Servant (type (:>), Post, JSON, Handler, (:<|>)(..))

import Database.Selda (tryCreateTable)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import PostgresConnectionSettings (connectionSettings)

import Entity.Translation (translationsTable)
import Entity.User (usersTable)
import Entity.Word (wordsTable)
import Entity.WordSet (wordSetsTable)
import Entity.WordSetWord (wordSetWordsTable)

type DBSchemaAPI = "schema" :> "init" :> Post '[JSON] Text

dbSchemaAPI = initSchema
  where
    initSchema :: Handler Text
    initSchema = do
      withPostgreSQL connectionSettings $ do
        tryCreateTable usersTable
        tryCreateTable wordsTable
        tryCreateTable wordsTable
        tryCreateTable wordSetsTable
        tryCreateTable wordSetWordsTable

      pure "Initialisation complete"
