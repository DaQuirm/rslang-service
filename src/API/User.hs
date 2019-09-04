{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.User where

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Handler, (:<|>)(..))
import Database.Selda (ID, query, select, insertWithPK)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import Entity.User (User, usersTable)

type UserAPI
  = "users" :>
    (    Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] (ID User)
    )

userAPI
  =    getUsers
  :<|> addUser
  where
    getUsers :: Handler [User]
    getUsers = withPostgreSQL ("lang" `on` "localhost") $ do
        query $ select usersTable

    addUser :: User -> Handler (ID User)
    addUser user = do
      withPostgreSQL ("lang" `on` "localhost") $ do
        insertWithPK usersTable [user]