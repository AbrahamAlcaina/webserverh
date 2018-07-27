{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger    (NoLoggingT(..), runNoLoggingT)
import Data.Maybe
import Data.Text               (pack)
import Data.Text.Lazy          (Text)
import Database.Persist.Sqlite
import Eventful.Store.Sqlite
import System.Environment
import Users                   (usersRoute)
import Web.Scotty.Trans        as S


main :: IO ()
main = do
  cfg <- getConfig
  let port = cfg ^. (appWebConfig . webPort)
  scottyT port (runApp cfg)  app

app :: ScottyT Text AppM ()
app = do
  usersRoute

getConfig::IO AppConfig
getConfig = do
  portM <- lookupEnv "PORT"
  fileDBM <- lookupEnv "DB_FILE"
  poolSizeM <- lookupEnv "DB_POOL_SIZE"
  let
    dbConfig = DBConfig (fromMaybe "data.db" fileDBM) (read (fromMaybe "10" poolSizeM))
    webConfig = WebConfig (read(fromMaybe "8080" portM))
  pool <- setupPool (dbConfig ^. fileDB) (dbConfig ^. poolSize)
  return $ AppConfig dbConfig webConfig pool

setupPool :: String -> Int -> IO ConnectionPool
setupPool fileDB poolSize =
    runNoLoggingT $ withSqlitePool (pack fileDB)  poolSize $ \pool -> NoLoggingT $ do
    initializeSqliteEventStore defaultSqlEventStoreConfig pool
    return pool
