module UserSpec (spec) where

import Test.Hspec
import Test.QuickCheck as Q

import Eventful (nil, uuidFromInteger)
import User

spec :: Spec
spec =
  describe "User" $
    it "property base test" $
      property prop_shouldAppyCommand


-- TODO generate random id/name
instance Arbitrary UserCommand where
  arbitrary = oneof
    [ return (CreateUser (uuidFromInteger 1) "Pepe")
    , return (RenameUser "Pepa")]

prop_shouldAppyCommand :: UserCommand -> Bool
prop_shouldAppyCommand c@(CreateUser uuid name) =
  case applyCommand initUser c of
    Right [UserCreated uuid name ] -> True
    _ -> False
prop_shouldAppyCommand c@(RenameUser name) =
  case applyCommand initUser c of
    Right [UserRenamed name ] -> True
    _ -> False
prop_shouldAppyCommand _ = False

