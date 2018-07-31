{-# LANGUAGE OverloadedStrings #-}

module Users (usersRoute, userErrorHandler) where

import Control.Monad.Reader
import Data.Text.Lazy       (Text)
import Web.Scotty.Trans

import App                       (AppM)
import Control.Monad.Catch       (Exception, catch)
import Data.String               (fromString)
import Debug.Trace
import Network.HTTP.Types.Status
import User.Model                (UserError(..))
import User.Service

-- Here use a lift becouse the callback of put/get is an ActionT from Scotty
updateUserName  ::  (ScottyError e, Exception e) => ScottyT e AppM ()
updateUserName = put "/users/:uuid/changeName/:name" $ do
    name <- param "name"
    uuid <- param "uuid"
    userState <- lift $ runCommand $ RenameUserServiceCommand uuid name
    json userState

createUser  ::  (ScottyError e) => ScottyT e AppM ()
createUser = put "/users/:name" $ do
  name <- param "name"
  userState <- lift $ runCommand $ CreateUserServiceCommand name
  json userState

getUser  ::  (ScottyError e) => ScottyT e AppM ()
getUser = get "/users/:uuid" $ do
  uuid <- param "uuid"
  userState <- lift $ runCommand $ GetUser uuid
  json userState

usersRoute ::  (ScottyError e, Exception e) => ScottyT e AppM ()
usersRoute  = do
  updateUserName
  createUser
  getUser

instance ScottyError UserError where
  stringError = read
  showError = fromString . show

userErrorHandler :: UserError -> ActionT UserError AppM ()
userErrorHandler UserNotFound = status notFound404
userErrorHandler UserAlreadyCreated = status conflict409
