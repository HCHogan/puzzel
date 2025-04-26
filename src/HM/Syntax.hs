{-# LANGUAGE TemplateHaskell #-}

module HM.Syntax where

import Control.Lens
import Control.Monad
import Data.Map qualified as M
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
  deriving (Show, Eq, Ord)

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

-- type Infer a = ExceptT TypeError (State Unique) a

newtype Unique = Unique {_count :: Int} deriving (Show, Eq)

makeLenses ''Unique

runInfer :: (Error TypeError :> es, State TypeEnv :> es) => Eff es (Subst, Type) -> Either TypeError Scheme
runInfer = undefined

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s (TArr a b) = TArr (apply s a) (apply s b)
  apply s t@(TVar a) = M.findWithDefault t a s

  ftv (TCon a) = S.empty
  ftv (TVar a) = S.singleton a
  ftv (TArr t1 t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
   where
    s' = foldr M.delete s as
  ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance (Substitutable a) => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: (State Unique :> es) => Eff es Type
fresh = do
  idx <- gets (^. count)
  modify (count +~ 1)
  return $ TVar $ TV (letters !! idx)

{-
  Unification rules for our little HM language:

    -- Uni-Const
    c ~ c : []

    -- Uni-Var
    α ~ α : []

    -- Uni-VarLeft  (requires α ∉ ftv(τ))
    α ~ τ : [τ/α]

    -- Uni-VarRight (requires α ∉ ftv(τ))
    τ ~ α : [τ/α]

    -- Uni-Con
    τ1 ~ τ1' : θ1
    [θ1] τ2 ~ [θ1] τ2' : θ2
    -------------------------
    τ1 τ2 ~ τ1' τ2' : θ2 ∘ θ1

    -- Uni-Arrow
    τ1 ~ τ1' : θ1
    [θ1] τ2 ~ [θ1] τ2' : θ2
    -----------------------------
    τ1 -> τ2 ~ τ1' -> τ2' : θ2 ∘ θ1
-}

