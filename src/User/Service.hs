module User.Service (runCommand, ServiceCommand(..)) where

import Control.Monad.Catch  (MonadThrow, throwM)
import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Eventful

import App        (HasAppConfig, appDbPool)
import AppError
import DB         (runDB)
import User.Model
  (UserCommand(..), UserState, userCommandHandler, userProjection)
import User.Store

data ServiceCommand
    = CreateUserServiceCommand String
    | RenameUserServiceCommand UUID String
    | GetUser UUID
    deriving (Show)

runCommand :: (MonadIO m, MonadReader r m, HasAppConfig r, MonadThrow m ) => ServiceCommand -> m UserState
runCommand (CreateUserServiceCommand name) = do
    uuid <- liftIO uuidNextRandom
    let command = CreateUser uuid name
    runDB $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState uuid
runCommand (RenameUserServiceCommand uuid name) = do
    let command = RenameUser name
    runDB $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState uuid
runCommand (GetUser uuid) = getUserState uuid

getUserState uuid = do
    latestStreamProjection <- runDB $
        getLatestStreamProjection userEventStoreReader (versionedStreamProjection uuid userProjection)
    return $ streamProjectionState latestStreamProjection


