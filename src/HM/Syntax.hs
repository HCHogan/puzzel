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

-- lookupEnv :: TypeEnv -> Var -> Maybe Scheme
-- lookupEnv (TypeEnv m) v = M.lookup m v

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv e) (x, s) = TypeEnv $ M.insert x s e

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable String
  deriving (Show, Eq)

type Subst = M.Map TVar Type

-- use effectful instead of:
-- type Infer a = ExceptT TypeError (State Unique) a

newtype Unique = Unique {_count :: Int} deriving (Show, Eq)

makeLenses ''Unique

runInfer :: (HasCallStack, Error TypeError :> es, State TypeEnv :> es) => Eff es (Subst, Type) -> Either TypeError Scheme
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

-- the type variable a must not occur free in tau.
-- note that unifying a -> b and a is exactly what we would have to do if we tried to type check the
-- omega combinator \x -> x x
occursCheck :: (Substitutable a) => TVar -> a -> Bool
occursCheck a s = S.member a (ftv s)

unify :: (HasCallStack, Error TypeError :> es) => Type -> Type -> Eff es Subst
-- Uni-Arrow
-- τ1 ~ τ1' : θ1 [θ1] τ2 ~ [θ1] τ2' : θ2
-- -----------------------------
-- τ1 -> τ2 ~ τ1' -> τ2' : θ2 ∘ θ1
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s1 `compose` s2)
-- Uni-Var   α ~ α : []
-- Uni-VarLeft  (requires α ∉ ftv(τ)  α ~ τ : [τ/α]
unify (TVar a) t = bind a t
-- Uni-VarRight (requires α ∉ ftv(τ)) τ ~ α : [τ/α]
unify t (TVar a) = bind a t
-- Uni-Const c ~ c : []
unify (TCon a) (TCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: (HasCallStack, Error TypeError :> es) => TVar -> Type -> Eff es Subst
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ M.singleton a t

-- Generalization and Instantiation (Hindley–Milner):
-- -- T-Inst
-- Γ ⊢ e : σ₁       σ₁ ⊑ σ₂
-- -----------------------------
-- Γ ⊢ e : σ₂
instantiate :: (State Unique :> es) => Scheme -> Eff es Type
instantiate (Forall as t) = do
  as' <- traverse (const fresh) as -- fresh need State Unique :> es here
  let s = M.fromList $ zip as as'
  return $ apply s t

--  -- T-Gen
--  Γ ⊢ e : σ        ᾱ ∉ ftv(Γ)
--  -----------------------------
--  Γ ⊢ e : ∀ ᾱ. σ
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
 where
  as = S.toList $ ftv t `S.difference` ftv env

-- Typing Rules for HM Type System:

-- (T-Let)
-- If Γ ⊢ e₁ : σ and Γ, x : σ ⊢ e₂ : τ, then Γ ⊢ let x = e₁ in e₂ : τ

-- (T-Gen)
-- If Γ ⊢ e : σ and α ∉ ftv(Γ), then Γ ⊢ e : ∀α. σ

-- (T-Inst)
-- If Γ ⊢ e : σ₁ and σ₁ ⊑ σ₂, then Γ ⊢ e : σ₂

lookupEnv :: (HasCallStack, State Unique :> es, Error TypeError :> es) => TypeEnv -> Var -> Eff es (Subst, Type)
lookupEnv (TypeEnv e) v = case M.lookup v e of
  Nothing -> throwError $ UnboundVariable (show v)
  Just s -> do
    t <- instantiate s
    return (nullSubst, t)

infer :: (HasCallStack, State Unique :> es, Error TypeError :> es) => TypeEnv -> Expr -> Eff es (Subst, Type)
infer env = \case
  -- (T-Var)
  -- If x : σ ∈ Γ, then Γ ⊢ x : σ
  Var x -> lookupEnv env x
  -- (T-Lam)
  -- If Γ, x : τ₁ ⊢ e : τ₂, then Γ ⊢ λx. e : τ₁ → τ₂
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)
  -- (T-App)
  -- If Γ ⊢ e₁ : τ₁ → τ₂ and Γ ⊢ e₂ : τ₁, then Γ ⊢ e₁ e₂ : τ₂
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (TArr t2 tv)
    undefined

