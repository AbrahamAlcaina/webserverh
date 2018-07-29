{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Maybe         (fromMaybe)
import System.Environment
import Web.Scotty.Trans

import App
import DB    (setupPool)
import Users (usersRoute)

main :: IO ()
main = do
  cfg <- getConfig
  let port = cfg ^. (appWebConfig . webPort)
  scottyT port (runApp cfg)  app

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
