module ES where

import Control.Concurrent.STM
import Eventful
import Eventful.Store.Memory

es = do
  tvar <- eventMapTVar
  let
    writer = tvarEventStoreWriter tvar
    reader = tvarEventStoreReader tvar
  return (writer, reader)
