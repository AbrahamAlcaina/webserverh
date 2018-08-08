module DB where

import Control.Lens            ((^.))
import Control.Monad.Logger    (NoLoggingT(..), runNoLoggingT)
import Control.Monad.Reader    (MonadReader, ask, liftIO)
import Data.Text               (pack)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool, withSqlitePool)

import Eventful.Store.Sqlite
  (defaultSqlEventStoreConfig, initializeSqliteEventStore)

import App (HasAppConfig, appDbPool)

getPool :: (MonadReader r m, HasAppConfig r ) => m ConnectionPool
getPool = do
    config <- ask
    return $ config ^. appDbPool

setupPool :: String -> Int -> IO ConnectionPool
setupPool fileDB poolSize =
    runNoLoggingT $ withSqlitePool (pack fileDB) poolSize $ \pool -> do
    initializeSqliteEventStore defaultSqlEventStoreConfig pool
    return pool

runDB c = do
    pool <- getPool
    liftIO $ runSqlPool c pool
