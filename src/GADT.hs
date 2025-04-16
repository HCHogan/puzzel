{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GADT () where

import Data.IntMap qualified as IM
import Data.Kind
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Prelude hiding (drop, head, indx, lookup, replicate, tail, take, zipWith, (++))

data Color = Red | Green | Blue

type Quantity :: Color -> Type
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

rankN :: (forall a. a -> Int) -> Int
rankN f = f 1 + f "hello"

-- example
-- vecIndex' (SSucc SZero) vecTwo -- 成功
-- vecIndex' (SSucc $ SSucc SZero) vecTwo -- 报错

class Key k where
  data Map k :: Type -> Type
  empty :: Map k v
  lookup :: k -> Map k v -> Maybe v

instance Key Bool where
  data Map Bool v = MapBool (Maybe v) (Maybe v)
  empty = MapBool Nothing Nothing
  lookup True (MapBool x _) = x
  lookup False (MapBool _ y) = y

instance (Key a, Key b) => Key (a, b) where
  data Map (a, b) v = MapPair (Map a (Map b v))
  empty = MapPair empty
  lookup (x, y) (MapPair m) = lookup x m >>= \m2 -> lookup y m2

instance (Key a) => Key [a] where
  data Map [a] v = MapList (Maybe v) (Map (a, [a]) v)
  empty = MapList Nothing empty
  lookup [] (MapList m0 _) = m0
  lookup (x : xs) (MapList _ m1) = lookup (x, xs) m1

-- can be think of a synatic sugar for a type level function:
-- data Map_Bool v = MapBool (Maybe v) (Maybe v)
-- instance Key Bool where
--   type Map Bool = Map_Bool
--   ...

-- we can also use external map implementation directly in our map:
instance Key Int where
  data Map Int v = MapInt (IM.IntMap v)
  empty = MapInt IM.empty
  lookup k (MapInt m) = IM.lookup k m

-- memo functions
class Memo k where
  data Table k :: Type -> Type
  toTable :: (k -> r) -> Table k r -- build the table
  fromTable :: Table k r -> k -> r -- get the memod function from the table

memo :: (Memo k) => (k -> r) -> k -> r
memo f = fromTable (toTable f)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

instance (Memo a) => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w)) -- w is the value for [], Table a (Table [a] w) is the value for (x:xs)
  toTable f = TList (f []) (toTable (\x -> toTable (\xs -> f (x : xs))))
  fromTable (TList t _) [] = t
  fromTable (TList _ t) (x : xs) = fromTable (fromTable t x) xs

instance Memo Int where
  data Table Int w = TInt (Table [Bool] w)
  toTable f = TInt (toTable (f . bitsToInt))
   where
    bitsToInt = error "TODO"
  fromTable (TInt t) n = fromTable t (intToBits n)
   where
    intToBits = error "TODO"

-- linear fib
fib :: Int -> Int
fib = memo fib'
 where
  fib' :: Int -> Int
  fib' 0 = 1
  fib' 1 = 1
  fib' n = fib (n - 1) + fib (n - 2)

-- class Elem a where
--   data [:a:]
--   index :: [:a:] -> Int -> a
--
-- when you see a class declaration:
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   ...
-- it's equivalent to:
-- data Num a = MkNum (a -> a -> a) (a -> a -> a) ...
-- (+) :: Num a -> a -> a -> a -- work as a selector for Num dictionary
-- (+) d = case d of MkNum plus _ _ -> plus
-- (-) :: Num a -> a -> a -> a
-- (-) d = case d of MkNum _ minus _ -> minus
-- this allow you to build big instances on small instances
--
-- when you wrote whis:
-- square :: Num n => n -> n
-- square n = n * n
--
-- instance Num Int where
--   (+) a b = plusInt a b
--   (-) a b = plusInt a b
--   ...
--
-- instance Eq a => Eq [a] where
--   (==) [] [] = True
--   (==) (x:xs) (y:ys) = x == y && xs == ys
--   (==) _ _ = False
--
-- the compiler generates this:
-- square :: Num n -> n -> n
-- square d n = (*) d n n
--
-- dNumInt :: Num Int
-- dNumInt = MkNum plusInt plusInt ...
--
-- dEqList :: Eq a -> Eq [a]
-- dEqList d = MkEq eql
--   where
--     eql [] [] = True
--     eql (x:xs) (y:ys) = (==) d x y && eql xs ys
--     eql _ _ = False
