module WebServer (getApplication) where

import Control.Error.Util   (bimapExceptT)
import Control.Exception    (try)
import Control.Monad.Except
import Servant

import API
import App

getApplication :: AppConfig -> Application
getApplication cfg = serve apiProxy $ getServer cfg

getServer :: AppConfig -> ServerT Api Handler
getServer cfg = hoistServer apiProxy (appMToHandler cfg) server

appMToHandler :: AppConfig -> AppM a -> Handler a
appMToHandler cfg =  Handler .  bimapExceptT errorToServantError id . ExceptT . try . runApp cfg
