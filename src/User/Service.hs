module User.Service where

import Database.Persist.Sqlite
import Eventful
import User.ES
import User.Model

runCommand :: Store -> UserCommand -> IO Store
runCommand store (CreateUser uuid name) = return store
runCommand store (RenameUser name) = return store
