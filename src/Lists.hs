{-# LANGUAGE GADTs #-}

module Lists () where

import Data.Dynamic
import Data.Kind
import Data.Type.Equality
import Type.Reflection

data Sigma :: (Type -> Type) -> Type where
  MkSigma :: p a -> a -> Sigma p

showIfBool :: Sigma TypeRep -> String
showIfBool (MkSigma p x) = case testEquality p (typeRep @Bool) of
  Just Refl -> show x
  Nothing -> "Not a Bool"

-- >>> let x = MkSigma TypeRep True
-- >>> let y = MkSigma TypeRep (4 :: Int)
-- >>> (showIfBool x, showIfBool y)
-- ("True","Not a Bool")

showIfBoolDynamic :: Dynamic -> String
showIfBoolDynamic dyn = case fromDynamic dyn of
  Just x -> if x then "True" else "False"
  Nothing -> "Not a Bool"

data Showable :: Type -> Type where
  WithShowable :: (Show a) => Showable a

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
