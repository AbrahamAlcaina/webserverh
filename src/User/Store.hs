module User.Store where

import Control.Monad.Reader
import Data.Aeson
import Database.Persist.Sql
import Eventful
import Eventful.Store.Sqlite

import User.Model

instance ToJSON UserEvent
instance FromJSON UserEvent

instance ToJSON UserState
instance FromJSON UserState

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

userEventStoreReader :: (MonadIO m) => VersionedEventStoreReader (SqlPersistT m) UserEvent
userEventStoreReader = serializedVersionedEventStoreReader jsonStringSerializer $ sqlEventStoreReader defaultSqlEventStoreConfig

userEventStoreWriter :: (MonadIO m) => VersionedEventStoreWriter (SqlPersistT m) UserEvent
userEventStoreWriter = synchronousEventBusWrapper writer handlers
  where
    sqlStore = sqliteEventStoreWriter defaultSqlEventStoreConfig
    writer = serializedEventStoreWriter jsonStringSerializer sqlStore
    handlers = []

userGlobalEventStoreReader :: (MonadIO m) => GlobalEventStoreReader (SqlPersistT m) UserEvent
userGlobalEventStoreReader =
  serializedGlobalEventStoreReader jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)
