module User.MemoryESSpec(spec) where

import Eventful
import Test.Hspec
import User.MemoryES
import User.Model

s = Store {
    sUpdate = \a uuid -> return (),
    sPoll = \uuid -> return initUser
}

commandAction :: UserCommand -> Action UserEvent
commandAction cmd _ s = return $ commandHandlerHandler userCommandHandler s cmd

newName = "Pablito"

spec :: Spec
spec = describe "Test in memory Event Sourcing" $ do
    context "New store" $ do
        it "should contains a init user" $ do
            store <- newInMemoryStore
            user <- sPoll store nil
            user `shouldBe` initUser
        context "Add a new user" $
            it "should had the new user" $ do
                store <- newInMemoryStore
                uuid <- uuidNextRandom
                sUpdate store (commandAction (CreateUser uuid newName)) uuid
                user <- sPoll store uuid
                name user `shouldBe` newName
    context "Update user" $
        it "should change the name" $ do
            store <- newInMemoryStore
            sUpdate store (commandAction (RenameUser newName)) nil
            user <- sPoll store nil
            name user `shouldBe` newName
