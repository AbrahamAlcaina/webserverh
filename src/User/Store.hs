module User.Store where

import Control.Monad.Reader  (MonadIO)
import Data.Aeson            (FromJSON, ToJSON)
import Database.Persist.Sql  (SqlPersistT)
import Eventful              (synchronousEventBusWrapper)
import Eventful.Store.Sqlite

import App        (HasAppConfig, appDbPool)
import User.Model

instance ToJSON UserEvent
instance FromJSON UserEvent

instance ToJSON UserState
instance FromJSON UserState

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
