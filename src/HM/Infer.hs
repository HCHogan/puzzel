{-# LANGUAGE TemplateHaskell #-}

module HM.Infer where

import Control.Lens
import Control.Monad
import Data.Map qualified as M
import Data.Set qualified as S
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.State.Static.Local

import HM.Syntax
import HM.Type
import HM.Infer.TypeError
import HM.Infer.TypeEnv
import HM.Infer.Subst

-- use effectful instead of:
-- type Infer a = ExceptT TypeError (State Unique) a

newtype Unique = Unique {_count :: Int} deriving (Show, Eq)

makeLenses ''Unique

runInfer :: (HasCallStack) => Eff [Error TypeError, State TypeEnv] (Subst, Type) -> Either TypeError Scheme
runInfer m = closeOver <$> runPureEff (evalState emptyTypeEnv $ runErrorNoCallStack m)

closeOver :: (Subst, Type) -> Scheme
closeOver (sub, ty) = normalize sc
 where
  sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize = undefined

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
unify t1 t2 = throwError_ $ UnificationFail t1 t2

bind :: (HasCallStack, Error TypeError :> es) => TVar -> Type -> Eff es Subst
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError_ $ InfiniteType a t
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

ops :: M.Map Binop Type
ops =
  M.fromList
    [ (Add, TArr typeInt (TArr typeInt typeInt))
    , (Mul, TArr typeInt (TArr typeInt typeInt))
    , (Sub, TArr typeInt (TArr typeInt typeInt))
    , (Eql, TArr typeInt (TArr typeInt typeBool))
    ]

lookupEnv :: (HasCallStack, State Unique :> es, Error TypeError :> es) => TypeEnv -> Var -> Eff es (Subst, Type)
lookupEnv (TypeEnv e) v = case M.lookup v e of
  Nothing -> throwError_ $ UnboundVariable (show v)
  Just s -> do
    t <- instantiate s
    return (nullSubst, t)

-- Typing Rules for HM Type System:
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
    s3 <- unify (apply s2 t1) (TArr t2 tv) -- unify t1 with t2 -> tv
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  -- (T-Let)
  -- If Γ ⊢ e₁ : σ and Γ, x : σ ⊢ e₂ : τ, then Γ ⊢ let x = e₁ in e₂ : τ
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t' = generalize env' t1 -- let-generalization
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s1 `compose` s2, t2)
  Op op e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2 -- e1 shouldn't influence e2's type inference
    tv <- fresh
    s3 <- unify (TArr t1 (TArr t2 tv)) (ops M.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)
  Fix e1 -> do
    (s1, t) <- infer env e1
    tv <- fresh
    s2 <- unify (TArr tv tv) t -- fix is just a function that takes itself as input
    return (s2, apply s1 tv)
  If cond tr fl -> do
    (s1, t1) <- infer env cond
    (s2, t2) <- infer env tr
    (s3, t3) <- infer env fl
    s4 <- unify t1 typeBool
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  Lit (LInt _) -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)

-- multiparameter function application
inferPrim :: (HasCallStack, Error TypeError :> es, State Unique :> es) => TypeEnv -> [Expr] -> Type -> Eff es (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
 where
  inferStep = undefined
