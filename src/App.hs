{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module App where

import Control.Lens.TH         (makeClassy)
import Control.Monad.Catch     (MonadThrow)
import Control.Monad.IO.Class  (MonadIO)
import Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import Database.Persist.Sqlite (ConnectionPool)

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
