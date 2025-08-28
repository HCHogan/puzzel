module Demo3 where

import Control.Monad.Except
import Control.Monad.State
import Effectful
import Effectful.Error.Static qualified as EE
import Effectful.State.Static.Local qualified as ES

ex1 :: StateT Int (Except String) ()
ex1 = do
  modify (+ 1)
  throwError "Fuck"

-- >>> runExcept $ runStateT ex1 0
-- Left "Fuck"

ex2 :: ExceptT String (State Int) ()
ex2 = do
  modify (+ 1)
  throwError "Fuck"

-- >>> runState (runExceptT ex2) 0
-- (Left "Fuck",1)

ex3 :: (ES.State Int :> es, EE.Error String :> es) => Eff es ()
ex3 = do
  ES.modify (+ 1)
  EE.throwError_ "Fuck"

test1 = runPureEff $ EE.runErrorNoCallStack $ ES.runState 0 ex3

test2 = runPureEff $ ES.runState 0 $ EE.runErrorNoCallStack ex3

-- >>> test1
-- Left "Fuck"

-- >>> test2
-- (Left "Fuck",1)
