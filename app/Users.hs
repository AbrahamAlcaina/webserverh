{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Users (usersServer, UsersApi) where

import App          (AppM)
import Servant
import User.Model   (UserState)
import User.Service

type UsersApi = "users" :> Capture "uuid" String :> "changeName" :> Capture "name" String :> Put '[JSON] UserState
        :<|> "users" :> Capture "name" String :> Put '[JSON] UserState
        :<|> "users" :> Capture "uuid" String :> Get '[JSON] UserState


updateUserName :: String -> String -> AppM UserState
updateUserName uuid name = runCommand $ RenameUserServiceCommand uuid name

createUser  ::  String -> AppM UserState
createUser name = runCommand $ CreateUserServiceCommand name

getUser  ::  String -> AppM  UserState
getUser uuid = runCommand $ GetUser uuid

usersServer :: ServerT UsersApi AppM
usersServer  = updateUserName
  :<|> createUser
  :<|> getUser
