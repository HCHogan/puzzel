{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Kinds () where

import Data.Kind (Constraint, Type)
import Data.Set qualified as Set
import Prelude hiding (Either (..), Monad (..), not)

not :: Bool -> Bool
not True = False
not False = True

type List :: Type -> Type
data List a = Nil | Cons a (List a)

x :: Int
x = 5

type Monad :: (Type -> Type) -> Constraint
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

-- Constraint is something on the LHS of =>

type ReaderT :: Type -> (Type -> Type) -> Type -> Type
newtype ReaderT e m a = MkReaderT {runReaderT :: e -> m a}

type MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class MonadTrans t where
  lift :: m a -> t m a

type C :: Constraint
class C where
  method :: a -> ()

f :: (C) => () -> ()
f () = method ()

type FunctorC :: (Type -> Constraint) -> (Type -> Constraint) -> (Type -> Type) -> Constraint
class FunctorC c1 c2 f | f -> c1 c2 where
  fmapC :: (c1 a, c2 b) => (a -> b) -> f a -> f b

class Always a
instance Always a

instance FunctorC Always Ord Set.Set where
  fmapC = Set.map

type IntMod :: Nat -> Type
newtype IntMod n = MkIntMod Int
  deriving Show
