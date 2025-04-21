module Kinds () where

import Data.Kind (Constraint, Type)
import Data.Proxy
import Data.Set qualified as Set
import GHC.TypeLits
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
newtype IntMod n = MkIntMod Integer
  deriving (Show)

instance (KnownNat n) => Num (IntMod n) where
  MkIntMod x + MkIntMod y = MkIntMod ((x + y) `mod` natVal (Proxy @n))
  MkIntMod x * MkIntMod y = MkIntMod ((x * y) `mod` natVal (Proxy @n))
  abs = id
  signum _ = 1
  fromInteger x = MkIntMod (fromInteger x `mod` natVal (Proxy @n))
  negate (MkIntMod x) = MkIntMod (negate x `mod` natVal (Proxy @n))

-- Phase distinction: compiletime vs runtime
-- Take a type level information and produce a runtime information
-- >>> :t natVal
-- natVal :: KnownNat n => proxy n -> Integer

-- >>> natVal (4 :: IntMod 5)
-- 5

-- lets write a typesafe vec
data MyNat = Z | S MyNat

data Vec :: MyNat -> Type -> Type where
  VNil :: Vec 'Z a
  VCons :: a -> Vec n a -> Vec ('S n) a

headVec :: Vec ('S n) a -> a
headVec (VCons x _) = x

exampleVec :: Vec ('S ('S 'Z)) Int
exampleVec = VCons 1 (VCons 2 VNil)

-- let's try this out
-- >>> headVec VNil
-- Couldn't match type 'Z with 'S n0_aADI[tau:1]
-- Expected: Vec ('S n0_aADI[tau:1]) a_aADJ[sk:1]
--   Actual: Vec 'Z a_aADJ[sk:1]
-- In the first argument of `headVec', namely `VNil'
-- In the expression: headVec VNil
-- In an equation for `it_aABF': it_aABF = headVec VNil

type family Sing :: k -> Type

class FromSing k where
  fromSing :: Sing (a :: k) -> k

data MyProxy a = MyProxy

data MyBox = forall a. MkT a

