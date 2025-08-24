module STM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

data Account = Account {accountId :: String, balance :: TVar Int}

newAccount :: String -> Int -> IO Account
newAccount name initBa = do
  bal <- newTVarIO initBa
  return $ Account name bal

transfer :: Account -> Account -> Int -> STM ()
transfer (Account _ from) (Account _ to) amount = do
  fromBal <- readTVar from
  toBal <- readTVar to
  if fromBal < toBal
    then retry
    else do
      writeTVar from $ fromBal - amount
      writeTVar to $ toBal + amount

main :: IO ()
main = do
  accountA <- newAccount "A" 1000
  accountA <- newAccount "B" 1000
  return ()
