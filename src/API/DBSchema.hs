{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module API.DBSchema where

import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as ByteString
import Data.Aeson (eitherDecode)
import Control.Monad.IO.Class (liftIO)

import Servant (type (:>), Post, JSON, Handler, (:<|>)(..))

import Database.Selda (tryCreateTable, insert_)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import ServiceState (tick)
import App (AppT, API, modifyState)

import Entity.Translation (translationsTable)
import Entity.User (usersTable)
import Entity.Word (wordsTable)
import Entity.WordSet (wordSetsTable)
import Entity.WordSetWord (wordSetWordsTable)
import Entity.Language (languagesTable)

type DBSchemaAPI = "schema" :> "init" :> Post '[JSON] Text

dbSchemaAPI :: API DBSchemaAPI
dbSchemaAPI = initSchema
  where
    initSchema :: AppT Handler Text
    initSchema = do
      modifyState $ tick "initSchema"

      tryCreateTable usersTable
      tryCreateTable wordsTable
      tryCreateTable wordsTable
      tryCreateTable wordSetsTable
      tryCreateTable wordSetWordsTable
      tryCreateTable languagesTable

      decoded <- eitherDecode <$> (liftIO $ ByteString.readFile "./data/languages.json")
      case decoded of
        Left error      -> pure $ "Could not decode languages.json" <> (pack error)
        Right languages -> do
          insert_ languagesTable languages
          pure "Initialisation complete"
