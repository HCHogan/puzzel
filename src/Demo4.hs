{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo4 where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (chr)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic qualified as ER
import Effectful.State.Static.Local qualified as ES
import GHC.Clock (getMonotonicTime)
import System.Timeout (timeout)

flakyAction1 :: StateT Int IO ()
flakyAction1 = do
  modify (+ 1)
  liftIO $ threadDelay 1_000_000

-- main :: IO ()
-- main = void $ flip runStateT 0 $ do
--   flakyAction1
--   get >>= liftIO . print
--
--   withRunInIO $ \runInIO -> timeout 1000000 (runInIO flakyAction1)
--   get >>= liftIO . print

flakyAction2 :: (ES.State Int :> es, IOE :> es) => Eff es ()
flakyAction2 = do
  ES.modify (+ 1)
  liftIO $ threadDelay 1_000_000

main2 :: IO ()
main2 = void $ runEff $ do
  ES.runState 0 $ do
    flakyAction2
    ES.get >>= liftIO . print -- prints 1
    withRunInIO $ \runInIO -> timeout 100_000 (runInIO flakyAction2)
    ES.get >>= liftIO . print -- prints 2

data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile s action = send (Profile s action)

runProfiling :: (IOE :> es) => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnlift env $ \unlift -> do
    t1 <- liftIO getMonotonicTime
    r <- unlift action
    t2 <- liftIO getMonotonicTime
    liftIO $ putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
    pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnlift env $ \unlift -> unlift action

action :: (Profiling :> es, IOE :> es) => Eff es ()
action = profile "greet" . liftIO $ putStrLn "Hello!"

main :: IO ()
main = runEff . runProfiling $ action

class (Monad m) => MonadRNG m where
  randomInt :: m Int

randomString :: (MonadRNG m) => Int -> m String
randomString n = map chr <$> replicateM n randomInt

data RNG :: Effect where
  RandomInt :: RNG m Int

type instance DispatchOf RNG = Dynamic

instance (RNG :> es) => MonadRNG (Eff es) where
  randomInt = send RandomInt

runDummyRNG :: Eff (RNG : es) a -> Eff es a
runDummyRNG = interpret_ \case
  RandomInt -> pure 42
