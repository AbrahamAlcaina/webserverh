{-# LANGUAGE GADTs                      #-}
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
import Servant.Server

import AppError (AppError)

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


newtype AppM a = AppM { unApp:: ReaderT AppConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO, MonadThrow)

runApp :: AppConfig -> AppM a -> IO a
runApp cfg = flip runReaderT cfg . unApp
