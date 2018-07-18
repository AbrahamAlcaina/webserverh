{-# LANGUAGE OverloadedStrings #-}

module Users where

import Data.Monoid ((<>))
import Web.Scotty

listUsers = get "/users" $ text "list"
createUser = post "/users" $ text "list"
updateUser = put "/users/:uuid" $ do
  uuid <- param "uuid"
  text $ "update" <> uuid <> "!"

usersRoute :: ScottyM ()
usersRoute = do
  listUsers
  createUser
  updateUser
