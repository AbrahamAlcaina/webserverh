module Utils (getConfig) where

import App
import DB  (setupPool)

port = 3000
file = "test/db/test.db"
--file = ":memory:"

size = 1
dbCfg = DBConfig  file size
webCfg = WebConfig port

getConfig :: IO AppConfig
getConfig = do
  pool <- setupPool file size
  return $ AppConfig dbCfg webCfg pool




