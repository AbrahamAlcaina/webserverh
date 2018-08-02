{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens             ((^.))
import Data.Maybe               (fromMaybe)
import Network.Wai.Handler.Warp (run)
import System.Environment       (lookupEnv)

import App
import DB        (setupPool)
import WebServer (getApplication)

main :: IO ()
main = do
  cfg <- getConfig
  let port = cfg ^. (appWebConfig . webPort)
  run port $ getApplication cfg

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

