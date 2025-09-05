{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Heftia where

import Control.Monad.Hefty
import Prelude hiding (log, span)

data Log :: Effect where
  Log :: String -> Log f ()

makeEffectF ''Log

data Span :: Effect where
  Span :: String -> f a -> Span f a

makeEffectH ''Span

runLog :: (Emb IO :> es) => Eff (Log : es) ~> Eff es
runLog = interpret \(Log msg) -> liftIO $ putStrLn $ "[Log]" <> msg

runSpan :: (Emb IO :> es) => Eff (Span : es) ~> Eff es
runSpan = interpret \(Span name m) -> do
  liftIO $ putStrLn $ "[Start span '" <> name <> "']"
  r <- m
  liftIO $ putStrLn $ "[End span '" <> name <> "']"
  pure r

prog :: IO ()
prog = runEff . runLog . runSpan $ do
  span "example program" do
    log "foo"
    span "greeting" do
      log "hello"
      log "world"

    log "bar"

