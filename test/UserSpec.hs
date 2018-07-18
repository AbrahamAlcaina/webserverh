{-# LANGUAGE NamedFieldPuns #-}
module UserSpec (spec) where

import Test.Hspec
import Test.QuickCheck as Q

import Eventful (nil, uuidFromInteger)
import User

spec :: Spec
spec =
  describe "User" $
    it "property base test" $
      property prop_shouldAppyCommandAndHandle

initUserUUID = uuid initUser

-- TODO generate random id/name
instance Arbitrary UserCommand where
  arbitrary = oneof
    [ return (CreateUser (uuidFromInteger 1) "Pepe")
    , return (RenameUser "Pepa")]

prop_shouldAppyCommandAndHandle :: UserCommand -> Bool
prop_shouldAppyCommandAndHandle c@(CreateUser uuid name) =
  case applyCommand initUser c of
    Right [UserCreated uuid name ] ->
      handleEvent initUser (UserCreated uuid name) == User{uuid, name}
    _ -> False
prop_shouldAppyCommandAndHandle c@(RenameUser name) =
  case applyCommand initUser c of
    Right [UserRenamed name ] ->
      handleEvent initUser (UserRenamed name) == User{uuid=initUserUUID, name}
    _ -> False
prop_shouldAppyCommandAndHandle _ = False

