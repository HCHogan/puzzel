module Demo4 where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Effectful
import Effectful.State.Static.Local qualified as ES
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
