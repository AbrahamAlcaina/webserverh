{-# LANGUAGE OverloadedStrings #-}

module Users where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid             ((<>))
import Database.Persist.Sqlite
import Eventful.Store.Sqlite
import User.Service            as Service
import Web.Scotty              as Scotty

updateUserName pool = Scotty.put "/users/:uuid/changeName/:name" $ do
  name <- param "name"
  uuid <- param "uuid"
  userState <- liftIO $ Service.runCommand pool $ RenameUserServiceCommand uuid name
  json userState
createUser pool = Scotty.put "/users/:name" $ do
  name <- param "name"
  userState <- liftIO $ Service.runCommand pool $ CreateUserServiceCommand name
  json userState
getUser pool = Scotty.get "/users/:uuid" $ do
  uuid <- param "uuid"
  userState <- liftIO $ Service.runCommand pool $ GetUser uuid
  json userState

usersRoute :: ConnectionPool -> ScottyM ()
usersRoute pool  = do
  updateUserName pool
  createUser pool
  getUser pool
