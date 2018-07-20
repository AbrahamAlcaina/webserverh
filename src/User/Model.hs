{-# LANGUAGE NamedFieldPuns #-}

module User.Model where

import Eventful

data UserState = UserState { uuid::UUID, name::String }
  deriving (Show, Eq)

data UserEvent = UserCreated UUID String
  | UserRenamed String
  deriving (Eq, Show, Read)

handleEvent :: UserState -> UserEvent -> UserState
handleEvent _ (UserCreated uid name) = createUser uid name
handleEvent user (UserRenamed newName) = renameUser user newName

createUser :: UUID -> String -> UserState
createUser uuid name = UserState{uuid, name}

renameUser :: UserState -> String -> UserState
renameUser UserState{uuid} name = UserState{uuid, name}

initUser :: UserState
initUser = UserState {uuid = nil, name = "" }

initialUserProjection :: Projection UserState UserEvent
initialUserProjection = Projection
  {
    projectionSeed = initUser
    ,projectionEventHandler = handleEvent
  }

data UserCommand = CreateUser UUID String
  | RenameUser String
  deriving (Eq, Show, Read)

data UserError = UserAlreadyCreated

applyCommand :: UserState -> UserCommand -> Either UserError [UserEvent]
applyCommand _ (CreateUser newId name) = if nil==newId then  Left UserAlreadyCreated else Right [UserCreated newId name]
applyCommand _ (RenameUser name) = Right [UserRenamed name]

adaptApplyCommand :: UserState -> UserCommand -> [UserEvent]
adaptApplyCommand = either (const []) id `compose` applyCommand
  where compose = (.).(.)

userCommandHandler :: CommandHandler UserState UserEvent UserCommand
userCommandHandler = CommandHandler adaptApplyCommand initialUserProjection
