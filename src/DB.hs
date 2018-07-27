module DB where

import Control.Lens
import Control.Monad.Reader
import Database.Persist.Sql

import App        (HasAppConfig, appDbPool)
import User.Model

getPool :: (MonadReader r m, HasAppConfig r ) => m ConnectionPool
getPool = do
    config <- ask
    return $ config ^. appDbPool

runDB c = do
    pool <- getPool
    liftIO $ runSqlPool c pool
