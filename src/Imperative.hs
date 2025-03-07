module Imperative (
  def,
  var,
  lit,
  while,
  (+=),
  (-=),
  (*=),
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (when)

data Value = Var Int | Lit Integer deriving (Show)
data Env = Env {nextId :: Int, vars :: Map Int Integer}

initialEnv :: Env
initialEnv = Env{nextId = 0, vars = M.empty}

evalValue :: Value -> State Env Integer
evalValue (Lit i) = return i
evalValue (Var id) = do
  env <- get
  case M.lookup id (vars env) of
    Just i -> return i
    Nothing -> error "var not found"

-- update state
modifyVar :: Value -> (Integer -> Integer) -> State Env ()
modifyVar (Lit i) _ = error "cant modify lit"
modifyVar (Var i) f = do
  env <- get
  put env{vars = M.adjust f i (vars env)}

-- use State Env a

-- update state, return Value
var :: Integer -> State Env Value
var n = do
  env <- get
  let i = nextId env
  put env{nextId = i + 1, vars = M.insert i n (vars env)}
  return $ Var i

lit :: Integer -> Value
lit = Lit

def :: State Env Value -> Integer
def m = evalState (m >>= evalValue) initialEnv

while :: Value -> (Integer -> Bool) -> State Env () -> State Env ()
while r f act = do
  v <- evalValue r
  when (f v) $ act *> while r f act

(+=) :: Value -> Value -> State Env ()
a += b = evalValue b >>= modifyVar a . (+)

(*=) :: Value -> Value -> State Env ()
a *= b = evalValue b >>= modifyVar a . (*)

(-=) :: Value -> Value -> State Env ()
a -= b = do
  y <- evalValue b
  modifyVar a (+ negate y)
