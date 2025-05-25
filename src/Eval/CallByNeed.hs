module Eval.CallByNeed where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Expr
  = Var Name
  | Lit Ground
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Show)

data Ground
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Type
  = TBool
  | TInt
  | TArr Type Type
  deriving (Eq, Show)

type Name = String

type Env = [(Name, Type)]

type VEnv = [(String, IORef Thunk)]

type Thunk = () -> IO Value

data Value
  = VBool Bool
  | VInt Int
  | VClosure (Thunk -> IO Value)

update :: IORef Thunk -> Value -> IO ()
update ref v = writeIORef ref (const $ return v)

force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v <- th ()
  update ref v
  return v

mkThunk :: VEnv -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body = \a -> do
  a' <- newIORef a
  eval ((x, a') : env) body

eval :: VEnv -> Expr -> IO Value
eval env ex = case ex of
  Var n -> do
    th <- lookupEnv env n
    v <- force th
    return v

-- Lam x e -> return $ VClosure (mkThunk env x e)

lookupEnv :: VEnv -> Name -> IO (IORef Thunk)
lookupEnv = undefined
