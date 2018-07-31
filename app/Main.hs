{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe             (fromMaybe)
import System.Environment     (lookupEnv)
import Web.Scotty.Trans

import App
import DB    (setupPool)
import Users (userErrorHandler, usersRoute)

main :: IO ()
main = do
  cfg <- getConfig
  let port = cfg ^. (appWebConfig . webPort)
  scottyT port (runApp cfg)  app

hex = do
  userErrorHandler

app = do
  defaultHandler hex
  usersRoute

getConfig::IO AppConfig
getConfig = do
  port <- fromMaybe "8080" <$> lookupEnv "PORT"
  file <- fromMaybe "data.db" <$> lookupEnv "DB_FILE"
  size <- fromMaybe "10"<$> lookupEnv "DB_POOL_SIZE"
  let
    dbConfig = DBConfig file (read size)
    webConfig = WebConfig (read port)
  pool <- setupPool (dbConfig ^. fileDB) (dbConfig ^. poolSize)
  return $ AppConfig dbConfig webConfig pool

