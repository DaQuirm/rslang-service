{-# LANGUAGE OverloadedStrings #-}

module PostgresConnectionSettings where

import Database.Selda.PostgreSQL (PGConnectInfo(..))

connectionSettings :: PGConnectInfo
connectionSettings = PGConnectInfo
  { pgHost = "pg.rslang"
  , pgPort = 5432
  , pgDatabase = "lang"
  , pgSchema = Nothing
  , pgUsername = Just "rslang-service"
  , pgPassword = Just "rslang"
  }