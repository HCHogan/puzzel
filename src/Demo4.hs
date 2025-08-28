module Demo4 where

import Control.Concurrent (threadDelay)
import Control.Monad.State
import System.Timeout (timeout)

type MyApp = StateT Int IO

flakyAction :: MyApp ()
flakyAction = do
  put 1
  liftIO $ putStrLn "state updated to 1"
  liftIO $ threadDelay 1_000_000
  liftIO $ putStrLn "complete"

main :: IO ()
main = do
  finalStateNoTimeout <- runStateT flakyAction 0
  putStrLn $ "result: " ++ show finalStateNoTimeout

  timeout 1000000 (runStateT flakyAction 0) >>= \case
    Nothing -> putStrLn "G"
    Just a -> print a
