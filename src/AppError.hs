{-# LANGUAGE GADTs #-}

module AppError where

import Control.Exception (Exception)
import User.Model        (UserError(..))

{- data AppError a where
  UserAlreadyCreated :: AppError UserError
  UserNotFound :: AppError UserError
 -}

-- todo Investigate with GADT o some kind o template how to do that
data AppError = NoError
  | UserAlreadyCreated
  | UserNotFound
  deriving (Show)

instance Exception AppError
