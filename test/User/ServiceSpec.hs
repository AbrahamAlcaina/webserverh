{-# LANGUAGE NamedFieldPuns #-}
module User.ServiceSpec (spec) where

import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Utils                   (getConfig)

import Control.Monad.Catch  (MonadThrow, throwM)
import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Eventful             (nil, uuidFromInteger)

import App          (AppConfig, AppM, HasAppConfig, runApp)
import User.Model
import User.Service

import Control.Monad (mapM)

spec :: Spec
spec = describe "property test" $ beforeAll getConfig $
    it "should handle commands and events" $ \cfg ->
        property $ prop_randomCommands cfg

prop_randomCommands :: AppConfig -> [ServiceCommand] -> Property
prop_randomCommands cfg cmds = monadicIO $ do
  results <- run $ mapM (runApp cfg . shouldAppyCommandAndHandle) cmds
  assert $ all (==True) results

shouldAppyCommandAndHandle c@(CreateUserServiceCommand name) = do
  UserState {uuid, name = newName } <- runCommand c
  return (name == newName)
shouldAppyCommandAndHandle c@(RenameUserServiceCommand uuid name) = do
  UserState { uuid = newUUID, name = newName } <- runCommand c
  return $ (name == newName) &&  (uuid == newUUID)

instance Arbitrary ServiceCommand where
  arbitrary = do
    baseId <- choose (1, 10000)
    name <- nonEmptyString
    frequency [(1, return $ RenameUserServiceCommand (uuidFromInteger baseId) name )
      , (2, return $ CreateUserServiceCommand name)]


nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary
