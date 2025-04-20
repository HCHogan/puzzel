{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Untyped.Repl where

import Untyped.Eval
import Untyped.Parser
import Untyped.Pretty
import Untyped.Syntax

-- import Control.Monad
-- import Control.Monad.Trans

import Control.Monad.Trans
import Data.Foldable (traverse_)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Dispatch.Static
import System.Console.Haskeline

showStep :: (IOE :> es) => (Int, Expr) -> Eff es ()
showStep (d, x) = do
  let str = (replicate d ' ') ++ "=> " ++ ppexpr x
  liftIO $ putStrLn str

process :: (IOE :> es) => String -> Eff es ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> liftIO $ print err
    Right ex -> do
      let (out, ~steps) = runEval ex
      traverse_ showStep steps
      liftIO $ print out

repl :: (IOE :> es) => InputT (Eff es) ()
repl = do
  mline <- getInputLine "Untyped> "
  case mline of
    Nothing -> outputStrLn "Goodbye."
    Just input -> (lift $ process input) *> repl

main :: IO ()
main = runEff $ runInputT defaultSettings repl

data Haskeline :: Effect where
  GetInputLine :: String -> Haskeline m (Maybe String)
  OutputStrLn :: String -> Haskeline m ()

type instance DispatchOf Haskeline = Dynamic

getInputLineE :: (HasCallStack, Haskeline :> es) => String -> Eff es (Maybe String)
getInputLineE = send . GetInputLine

outputStrLnE :: (HasCallStack, Haskeline :> es) => String -> Eff es ()
outputStrLnE = send . OutputStrLn

-- runHaskeline :: (IOE :> es) => Eff (Haskeline : es) a -> Eff es a
-- runHaskeline eff = do
--   withRunInIO 

-- 在 Eff es 上提供接口
-- getInputLineE :: Haskeline :> es => String -> Eff es (Maybe String)
-- getInputLineE = send GetInputLine
--
-- outputStrLnE :: Haskeline :> es => String -> Eff es ()
-- outputStrLnE = send . OutputStrLn
