module User.Service (runCommand, ServiceCommand(..)) where

import           Control.Monad.Catch  (MonadThrow, throwM)
import           Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import qualified Data.Text            as T
import           Eventful

import App        (HasAppConfig, appDbPool)
import AppError
import DB         (runDB)
import User.Model
  (UserCommand(..), UserState, userCommandHandler, userProjection)
import User.Store

data ServiceCommand
    = CreateUserServiceCommand String
    | RenameUserServiceCommand String String
    | GetUser String
    deriving (Show)

runCommand :: (MonadIO m, MonadReader r m, HasAppConfig r, MonadThrow m ) => ServiceCommand -> m UserState
runCommand (CreateUserServiceCommand name) = do
    uuid <- liftIO uuidNextRandom
    let command = CreateUser uuid name
    runDB $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState $ show uuid
runCommand (RenameUserServiceCommand uid name) = do
    uuid <- getId uid
    let command = RenameUser name
    runDB $ applyCommandHandler userEventStoreWriter userEventStoreReader userCommandHandler uuid command
    getUserState uid
runCommand (GetUser uid) = getUserState uid

getUserState uid = do
    uuid <- getId uid
    latestStreamProjection <- runDB $
        getLatestStreamProjection userEventStoreReader (versionedStreamProjection uuid userProjection)
    return $ streamProjectionState latestStreamProjection

getId:: (MonadThrow m) => String -> m UUID
getId uid = case uuidFromText $ T.pack uid of
    (Just uuid) -> return uuid
    Nothing -> throwM UserNotFound


