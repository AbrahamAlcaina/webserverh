module User.ESSpec(spec) where

import Eventful
import Test.Hspec
import User.ES
import User.Model

s = Store {
    sUpdate = \a uuid -> return (),
    sPoll = \uuid -> return initUser
}

commandAction :: UserCommand -> Action UserEvent
commandAction cmd = \_ s -> do
    return $ commandHandlerHandler userCommandHandler s cmd

newName = "Pablito"

spec :: Spec
spec = describe "Test for in memory Event Sourcing" $ do
    it "New store should contains a init user" $ do
        store <- newInMemoryStore
        user <- sPoll store nil
        user `shouldBe` initUser
    it "Update user" $ do
        store <- newInMemoryStore
        sUpdate store (commandAction (RenameUser newName)) nil
        user <- sPoll store nil
        name user `shouldBe` newName
