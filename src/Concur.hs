{-# LANGUAGE BlockArguments #-}

module Concur () where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
import System.IO

data MyResource = MyResource

acquire :: IO MyResource
acquire = putStrLn "open" $> MyResource

release :: MyResource -> IO ()
release res = putStrLn "closed"

main :: IO ()
main = bracket acquire release $ \res -> do
  mutex <- newMVar res

  let worker i = withMVar mutex $ \r -> do
        putStrLn $ "thread " ++ show i ++ " is using MyResource"

  mapConcurrently_ worker [1 .. 5]

newtype DbConn = DbConn String

openDb :: IO DbConn
openDb = putStrLn "connected " $> DbConn "mydb"

closeDb :: DbConn -> IO ()
closeDb (DbConn conn) = putStrLn "closed " *> print conn

data DbMessage
  = Query String (MVar String)
  | Close

dbActor :: TQueue DbMessage -> DbConn -> IO ()
dbActor queue conn = do
  atomically (readTQueue queue) >>= \case
    Query sql responseMVar -> do
      putStrLn $ "Actor is querying: " ++ sql
      let result = "Result of (" ++ sql ++ ")"
      putMVar responseMVar result
      dbActor queue conn
    Close -> do
      putStrLn "Actor Closing"

test :: IO ()
test = withFile "test.txt" ReadMode \handle -> do
  contents <- hGetContents handle
  putStrLn contents


