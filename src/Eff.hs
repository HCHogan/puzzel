{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Eff () where

import Control.Lens
import Effectful
import Effectful.Dispatch.Static (HasCallStack)
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local

data User = User {_name :: String, _age :: Int} deriving (Show)
data AppState = AppState {_user :: User, _count :: Int} deriving (Show)

makeLenses ''User
makeLenses ''AppState

updateUser :: (State User :> es) => Eff es ()
updateUser = do
  modify (name .~ "Bob")
  modify (age %~ (+ 1))

getName :: (State User :> es) => Eff es String
getName = gets (view name)

modifyUserName :: (State AppState :> es) => Eff es ()
modifyUserName = modify (user . name .~ "Charlie")

emain :: IO ()
emain = runEff $ do
  let initialState = AppState (User "Alice" 30) 1
  finalState <- runState initialState modifyUserName
  liftIO $ print finalState

testWriter' :: (HasCallStack, Writer String :> es) => String -> Eff es ()
testWriter' s = tell "fuck"

testWriter :: (HasCallStack, Writer String :> es) => String -> Eff es ()
testWriter s = tell "fuck"

testStateError :: (HasCallStack, Writer String :> es, Error String :> es) => String -> Eff es ()
testStateError s = do
  tell "fuck"
  testWriter "bitch"
  throwError "shit"

runShit :: (HasCallStack) => (Either String (), String)
runShit = runPureEff $ runWriter $ runErrorNoCallStack $ testStateError "landepen"
