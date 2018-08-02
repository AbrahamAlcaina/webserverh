{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Error.Util
import Control.Exception        (catch, try)
import Control.Lens             ((^.))
import Control.Monad.Except
import Data.Maybe               (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Servant.Server
import System.Environment       (lookupEnv)

import API (Api, apiProxy, errorToServantError, server)
import App
import DB  (setupPool)

main :: IO ()
main = do
  cfg <- getConfig
  let port = cfg ^. (appWebConfig . webPort)
  run port $ getApplication cfg



getApplication :: AppConfig -> Application
getApplication cfg = serve apiProxy $ getServer cfg

getServer :: AppConfig -> ServerT Api Handler
getServer cfg = hoistServer apiProxy (appMToHandler cfg) server

appMToHandler :: AppConfig -> AppM a -> Handler a
appMToHandler cfg =  Handler .  bimapExceptT errorToServantError id . ExceptT . try . runApp cfg

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

