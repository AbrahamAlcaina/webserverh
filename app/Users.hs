{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Users (usersServer, UsersApi) where

import Eventful (UUID)
import Servant

import App          (AppM)
import User.Model   (UserState)
import User.Service

type UsersApi = "users" :> Capture "uuid" UUID :> "changeName" :> Capture "name" String :> Put '[JSON] UserState
        :<|> "users" :> Capture "name" String :> Put '[JSON] UserState
        :<|> "users" :> Capture "uuid" UUID :> Get '[JSON] UserState


updateUserName :: UUID -> String -> AppM UserState
updateUserName uuid name = runCommand $ RenameUserServiceCommand uuid name

createUser  ::  String -> AppM UserState
createUser name = runCommand $ CreateUserServiceCommand name

getUser  ::  UUID -> AppM  UserState
getUser uuid = runCommand $ GetUser uuid

usersServer :: ServerT UsersApi AppM
usersServer  = updateUserName
  :<|> createUser
  :<|> getUser
