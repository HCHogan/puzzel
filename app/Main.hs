module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- import Puzzel (answers)

main :: IO ()
main = forever $ do
  putStrLn "What is your question?"
  _ <- getLine
  threadDelay 1000000
  putStrLn "busy"
