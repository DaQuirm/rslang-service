{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.User where

import Servant (type (:>), type (:<|>), Get, Post, JSON, ReqBody, Handler, (:<|>)(..))
import Database.Selda (ID, query, select, insertWithPK)
import Database.Selda.PostgreSQL (withPostgreSQL, on)

import ServiceState (tick)
import App (AppT, API, modifyState)
import Entity.User (User, usersTable)

type UserAPI
  = "users" :>
    (    Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] (ID User)
    )

userAPI :: API UserAPI
userAPI
  =    getUsers
  :<|> addUser
  where
    getUsers :: AppT Handler [User]
    getUsers = do
      modifyState $ tick "getUsers"
      query $ select usersTable

    addUser :: User -> AppT Handler (ID User)
    addUser user = do
      modifyState $ tick "addUser"
      insertWithPK usersTable [user]