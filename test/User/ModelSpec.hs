{-# LANGUAGE NamedFieldPuns #-}
module User.ModelSpec (spec) where

import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Eventful   (nil, uuidFromInteger)
import User.Model

spec :: Spec
spec =
  describe "property test" $
      it "should handle commands and events" $
      property prop_shouldAppyCommandAndHandle -- verboseCheck $

initUserUUID = uuid initUser

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary

instance Arbitrary UserCommand where
  arbitrary = do
    baseId <- choose (1,100000)
    name <- nonEmptyString
    oneof [ return (CreateUser (uuidFromInteger baseId) name)
          , return (RenameUser name)]

prop_shouldAppyCommandAndHandle :: UserCommand -> Bool
prop_shouldAppyCommandAndHandle c@(CreateUser uuid name) =
  case applyCommand initUser c of
    Right [UserCreated uuid name ] ->
      handleEvent initUser (UserCreated uuid name) == UserState{uuid, name}
    _ -> False
prop_shouldAppyCommandAndHandle c@(RenameUser name) =
  case applyCommand initUser c of
    Right [UserRenamed name ] ->
      handleEvent initUser (UserRenamed name) == UserState{uuid=initUserUUID, name}
    _ -> False

