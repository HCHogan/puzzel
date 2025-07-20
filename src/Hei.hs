{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module Hei where

import Control.Arrow ((>>>))
import Control.Monad (forever, void, when)
import Control.Monad.Hefty
import Control.Monad.Hefty.Concurrent.Stream
import Control.Monad.Hefty.Concurrent.Timer
import Control.Monad.Hefty.Except
import Control.Monad.Hefty.Unlift
import Data.Foldable (for_)
import UnliftIO (bracket_)

-- | Generates a sequence of 1, 2, 3, 4 at 0.5-second intervals.
produce :: (Output Int :> es, Timer :> es, FOEs es) => Eff es ()
produce = void . runThrow @() $
  for_ [1 ..] \(i :: Int) -> do
    when (i == 5) $ throw ()
    output i
    sleep 0.5

-- | Receives the sequence at 0.5-second intervals and prints it.
consume :: (Input Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
consume = forever do
  liftIO . print =<< input @Int
  sleep 0.5

-- | Transforms by receiving the sequence as input at 0.5-second intervals,
--   adds 100, and outputs it.
plus100 :: (Input Int :> es, Output Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
plus100 = forever do
  i <- input @Int
  let o = i + 100
  liftIO $ putStrLn $ "Transform " <> show i <> " to " <> show o
  output o
  sleep 0.5

main :: IO ()
main = runUnliftIO . runTimerIO $ do
  let produceWithBracket =
        bracket_
          (liftIO $ putStrLn "Start")
          (liftIO $ putStrLn "End")
          (onlyFOEs produce)

  runMachineryIO_ $
    Unit @() @Int do
      produceWithBracket
      produceWithBracket
      >>> Unit @Int @Int plus100
      >>> Unit @Int @() consume
