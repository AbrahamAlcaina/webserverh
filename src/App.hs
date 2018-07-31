{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module App where

import Control.Lens.TH
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text               (Text)
import Database.Persist.Sqlite

data AppConfig = AppConfig {
  _appDbConfig    ::DBConfig
  , _appWebConfig ::WebConfig
  , _appDbPool    ::ConnectionPool
}

data DBConfig = DBConfig {
  _fileDB     :: String
  , _poolSize :: Int
}

data WebConfig = WebConfig {
  _webPort::Int
}

makeClassy ''AppConfig
makeClassy ''WebConfig
makeClassy ''DBConfig

data AppError = DBError
  | AppErr

newtype AppM a = AppM { unApp:: ReaderT AppConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO, MonadThrow)

makeClassyPrisms ''AppError

runApp :: AppConfig -> AppM a -> IO a
runApp e = flip runReaderT e . unApp
