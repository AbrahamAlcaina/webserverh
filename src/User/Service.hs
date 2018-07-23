module User.Service where
import           Control.Monad
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Eventful

import User.Model
import User.Store


data ServiceCommand
    = CreateUserServiceCommand String
    | RenameUserServiceCommand String String
    | GetUser String
    deriving (Show)

runCommand :: ConnectionPool ->ServiceCommand -> IO UserState
runCommand pool (CreateUserServiceCommand name) = do
    uuid <- uuidNextRandom
    let command = CreateUser uuid name
    runDB pool $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState pool $ show uuid
runCommand pool (RenameUserServiceCommand uid name) = do
    let uuid = getId uid
        command = RenameUser name
    runDB pool $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState pool uid
runCommand pool (GetUser uid) = getUserState pool uid

getUserState pool uid = do
  let uuid = getId uid
  latestStreamProjection <- runDB pool $
    getLatestStreamProjection userEventStoreReader (versionedStreamProjection uuid userProjection)
  return $ streamProjectionState latestStreamProjection

getId uid = case uuidFromText $ T.pack uid of
  (Just uuid) -> uuid
  Nothing -> nil
