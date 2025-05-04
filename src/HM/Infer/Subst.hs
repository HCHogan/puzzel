module HM.Infer.Subst where

import HM.Type
import HM.Infer.TypeEnv
import Data.Map qualified as M
import Data.Set qualified as S

type Subst = M.Map TVar Type

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

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `S.union` ftv b
