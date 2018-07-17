{-# LANGUAGE NamedFieldPuns #-}

module User (
  User(..)
  , UserEvent(..)
  , UserCommand(..)
  , userProjection
  , userAggregate) where

import Eventful

data User = User { uuid::UUID, name::String }
  deriving (Show, Eq)

data UserEvent = UserCreated UUID String
  | UserRenamed String
  deriving (Eq, Show, Read)

handleEvent :: User -> UserEvent -> User
handleEvent _ (UserCreated uid name) = createUser uid name
handleEvent user (UserRenamed newName) = renameUser user newName

createUser :: UUID -> String -> User
createUser uuid name = User{uuid, name}

renameUser :: User -> String -> User
renameUser User{uuid} name = User{uuid, name}

userProjection :: Projection User UserEvent
userProjection = Projection
  {
    projectionSeed  = User {uuid=nil, name=""}
    ,projectionEventHandler = handleEvent
  }

data UserCommand = CreateUser UUID String
  | RenameUser String
  deriving (Eq, Show, Read)

data UserError = UserAlreadyCreated

applyCommand::User -> UserCommand -> Either UserError [UserEvent]
applyCommand User{uuid} (CreateUser newId name) = if nil==uuid then Right [UserCreated newId name] else Left UserAlreadyCreated
applyCommand _ (RenameUser name) = Right [UserRenamed name]

adaptApplyCommand = either (const []) id `compose` applyCommand
  where compose = (.).(.)

userAggregate :: Aggregate User UserEvent UserCommand
userAggregate = Aggregate {
  aggregateCommandHandler = adaptApplyCommand
  , aggregateProjection = userProjection
}
