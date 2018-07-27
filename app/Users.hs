{-# LANGUAGE OverloadedStrings #-}

module Users (usersRoute) where

import Control.Monad.Reader
import Data.Text.Lazy       (Text)
import Web.Scotty.Trans

import App          (AppM)
import User.Service

-- Here use a lift becouse the callback of put/get is an ActionT from Scotty
updateUserName = put "/users/:uuid/changeName/:name" $ do
    name <- param "name"
    uuid <- param "uuid"
    userState <- lift $ runCommand $ RenameUserServiceCommand uuid name
    json userState

createUser = put "/users/:name" $ do
  name <- param "name"
  userState <- lift $ runCommand $ CreateUserServiceCommand name
  json userState

getUser = get "/users/:uuid" $ do
  uuid <- param "uuid"
  userState <- lift $ runCommand $ GetUser uuid
  json userState

usersRoute ::  ScottyT Text AppM ()
usersRoute  = do
  updateUserName
  createUser
  getUser
