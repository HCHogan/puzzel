module STLC.Stlc where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.Reader.Static

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

type Check = Eff '[Reader Env, Error TypeError]

data TypeError
  = TypeMismatch Type Type
  | NotFunction Type
  | NotInScope Name
  deriving (Show, Eq)

extend :: (Name, Type) -> Env -> Env
extend = (:)

inEnv :: (Name, Type) -> Check a -> Check a
inEnv p = local (extend p)

lookupVar :: Name -> Check Type
lookupVar n = do
  env <- ask
  case lookup n env of
    Just e -> return e
    Nothing -> throwError $ NotInScope n

check :: Expr -> Check Type
check (Lit (LInt _)) = return TInt
check (Lit (LBool _)) = return TBool
check (Lam x t e) = do
  rhs <- inEnv (x, t) (check e)
  return (TArr t rhs)
check (App e1 e2) = do
  t1 <- check e1
  t2 <- check e2
  case t1 of
    (TArr a b)
      | a == t2 -> return b
      | otherwise -> throwError $ TypeMismatch t2 a
    _ -> throwError $ NotFunction t1
check (Var x) = lookupVar x

runCheck :: Env -> Expr -> Either TypeError Type
runCheck env expr = runPureEff $ runErrorNoCallStack $ runReader env $ check expr

type VEnv = [Value]

data Value
  = VInt Int
  | VBool Bool
  | VClosure Expr VEnv

emptyEnv :: VEnv
emptyEnv = []


