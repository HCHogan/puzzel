module HM.Syntax where

import Data.HashMap.Strict qualified as M
import Data.Set qualified as S
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.State.Static.Local

type Var = String

data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Show, Eq, Ord)

-- parser is trivial
data Program = Program [Decl] Expr deriving (Eq)

type Decl = (String, Expr)

newtype TVar = TV String
  deriving (Show, Eq)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq)

infixr 9 `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

newtype TypeEnv = TypeEnv (M.Map Var Scheme)
  deriving (Semigroup, Monoid)

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv e) (x, s) = TypeEnv $ M.insert x s e

data TypeError

type Subst = M.Map TVar Type

runInfer :: (Error TypeError :> es, State TypeEnv :> es) => Eff es (Subst, Type) -> Either TypeError Scheme
runInfer = undefined

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set TVar


