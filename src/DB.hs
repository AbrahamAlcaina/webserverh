module DB where

import Control.Lens
import Control.Monad.Logger    (NoLoggingT(..), runNoLoggingT)
import Control.Monad.Reader
import Data.Text               (pack)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Eventful.Store.Sqlite

import App        (HasAppConfig, appDbPool)
import User.Model

getPool :: (MonadReader r m, HasAppConfig r ) => m ConnectionPool
getPool = do
    config <- ask
    return $ config ^. appDbPool

setupPool :: String -> Int -> IO ConnectionPool
setupPool fileDB poolSize =
    runNoLoggingT $ withSqlitePool (pack fileDB)  poolSize $ \pool -> NoLoggingT $ do
    initializeSqliteEventStore defaultSqlEventStoreConfig pool
    return pool

runDB c = do
    pool <- getPool
    liftIO $ runSqlPool c pool
