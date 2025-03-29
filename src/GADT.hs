{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GADT () where

import Data.Kind
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Prelude hiding (drop, head, indx, replicate, tail, take, zipWith, (++))

data Color = Red | Green | Blue

newtype Quantity (unit :: Color) = Quantity Double

addQuantity :: Quantity u -> Quantity u -> Quantity u
addQuantity (Quantity x) (Quantity y) = Quantity (x + y)

_ = addQuantity (Quantity 1 :: Quantity 'Red) (Quantity 2 :: Quantity 'Red)

data Nat = Zero | Succ Nat

-- equivalent to following without DataKinds:
-- data Zero
-- data Succ n
-- data Nat = Zero | Succ n

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

vecIndex' :: Nat -> Vec x b -> x
vecIndex' Zero (VCons x _) = x
vecIndex' (Succ n) (VCons _ xs) = vecIndex' n xs

type family (a :: Nat) :< (b :: Nat) where
  m :< 'Zero = 'False
  'Zero :< 'Succ n = 'True
  'Succ m :< 'Succ n = m :< n

type family (a :: Bool) || (b :: Bool) where
  'True || _ = 'True
  'False || b = b

type family (a :: Nat) :<= (b :: Nat) :: Bool where
  a :<= b = (a :< b) || (a == b)

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

type family Minus (n :: Nat) (m :: Nat) :: Nat where
  Minus n 'Zero = n
  Minus ('Succ n) ('Succ m) = Minus n m

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

index :: ((a :< b) ~ 'True) => SNat a -> Vec x b -> x
index SZero (VCons x _) = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate s (SSucc n) = VCons s (replicate s n)

zipWith :: ((x == y) ~ 'True) => (a -> b -> c) -> Vec a x -> Vec b y -> Vec c x
zipWith f VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

take :: ((x :<= y) ~ 'True) => SNat x -> Vec a y -> Vec a x
take SZero _ = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

drop :: ((x :<= y) ~ 'True) => SNat x -> Vec a y -> Vec a (Minus y x)
drop SZero xs = xs
drop (SSucc n) (VCons x xs) = drop n xs

head :: (('Zero :< y) ~ 'True) => Vec a y -> a
head (VCons x xs) = x

tail :: (('Zero :< y) ~ 'True) => Vec a y -> Vec a (Minus y ('Succ 'Zero))
tail (VCons x xs) = xs

(++) :: Vec a x -> Vec a y -> Vec a (Add x y)
(++) VNil x = x
(++) (VCons x xs) y = VCons x (xs ++ y)

data T a where
  T1 :: Bool -> T Bool
  T2 :: T a

f :: T a -> a -> a
f x y = case x of
  T1 z -> True
  T2 -> y

-- example
-- vecIndex' (SSucc SZero) vecTwo -- 成功
-- vecIndex' (SSucc $ SSucc SZero) vecTwo -- 报错
