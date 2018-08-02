module API (Api, apiProxy, server, errorToServantError) where

import Servant

import App      (AppM)
import AppError (AppError(..))
import Users    (UsersApi, usersServer)

-- to include use :<|>
type Api = UsersApi

apiProxy :: Proxy Api
apiProxy = Proxy

server :: ServerT Api AppM
server = usersServer

errorToServantError :: AppError -> ServantErr
errorToServantError UserAlreadyCreated = err409
errorToServantError UserNotFound = err404
