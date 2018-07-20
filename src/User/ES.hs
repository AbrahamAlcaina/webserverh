module User.ES where

import User.Model

import Control.Monad
import Control.Monad.STM

import Eventful
import Eventful.Store.Memory

-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action e = UUID -> UserState -> IO [e]

data Store = Store
  { sUpdate :: Action UserEvent -> UUID -> IO ()
  , sPoll   :: UUID -> IO UserState
  }

newStoreFrom
  :: (UUID -> [UserEvent] -> IO ())
  -> (UUID -> IO (
          StreamProjection UUID EventVersion UserState UserEvent)
      )
  -> IO Store
newStoreFrom write getLatestUserProjection = do
  let update a uuid =
          getLatestState uuid >>= a uuid >>= write uuid
      getLatestState uuid =
          streamProjectionState <$> getLatestUserProjection uuid
  return $ Store update getLatestState

newInMemoryStore :: IO Store
newInMemoryStore = do
  -- This is just like the setup in counter-cli's main:
  tvar <- eventMapTVar
  let
    w = tvarEventStoreWriter tvar
    r = tvarEventStoreReader tvar
  newStoreFrom
    (\uuid -> void . atomically . storeEvents w uuid AnyPosition)
    (\uuid -> atomically $ getLatestStreamProjection r $
      versionedStreamProjection uuid initialUserProjection)
