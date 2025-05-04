module HM.Repl where

import HM.Eval
import HM.Infer.TypeEnv qualified as Env
import HM.InferC
import HM.Parser
import HM.Pretty
import HM.Syntax

import Data.Map qualified as Map
import Data.Monoid
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.IO qualified as L

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.List (foldl', isPrefixOf)

import System.Console.Repline hiding (options)
import System.Environment
import System.Exit

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { tyctx :: Env.TypeEnv -- Type environment
  , tmctx :: TermEnv -- Value environment
  }

initState :: IState
initState = IState Env.emptyTypeEnv emptyTmenv

type Repl = HaskelineT (StateT IState IO)
hoistErr :: (Show e) => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
 where
  (val, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  mod <- hoistErr $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  tyctx' <- hoistErr $ inferTop (tyctx st) mod

  -- Create the new environment
  let st' =
        st
          { tmctx = foldl' evalDef (tmctx st) mod
          , tyctx = tyctx' <> (tyctx st)
          }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  case lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (tmctx st') "it" ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case Env.lookup "it" (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Env.lookup arg (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> exec False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: (MonadIO m) => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":load", fileCompleter)
  -- , (":type"  , values)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  Env.TypeEnv ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options' :: [(String, [String] -> Repl ())]
options' =
  [ ("load", load)
  , ("browse", browse)
  , ("quit", quit)
  , ("type", typeof)
  ]

options :: Options Repl
options =
  [ ("load", \arg -> load (words arg))
  , ("browse", \_ -> browse [])
  , ("quit", \_ -> quit ())
  , ("type", \arg -> typeof (words arg))
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
    evalRepl
      (\_ -> pure "HM> ") -- 1. banner
      cmd -- 2. command handler
      options -- 3. options table
      (Just ':') -- 4. prefix character
      Nothing -- 5. no multi-line trigger
      completer -- 6. tab completer
      pre -- 7. initialiser (Repl ())
      finaliser -- 8. finaliser
 where
  finaliser :: Repl ExitDecision
  finaliser = do
    liftIO $ putStrLn "Goodbye!"
    return Exit

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> shell (load [fname])
    ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"
