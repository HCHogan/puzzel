{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module LevityPoly where

import Data.Kind
import GHC.Exts

data Parity = Even | Odd
  deriving (Eq, Show)

instance Num Parity where
  Even + Even = Even
  Even + Odd = Odd
  Odd + Even = Odd
  Odd + Odd = Even

  p1 - p2 = p1 + p2

  Odd * Odd = Odd
  _ * _ = Even

  negate Even = Even
  negate Odd = Odd

  abs x = x

  signum Even = 0
  signum Odd = 1

  fromInteger n =
    if even n
      then Even
      else Odd

data ParityInteger = MkPi Parity Integer
  deriving (Eq, Show)

piFromInteger :: Integer -> ParityInteger
piFromInteger n
  | even n = MkPi Even n
  | otherwise = MkPi Odd n

add :: ParityInteger -> ParityInteger -> ParityInteger
add (MkPi p1 n1) (MkPi p2 n2) = MkPi (p1 + p2) (n1 + n2)

bigPI :: ParityInteger
bigPI = go (piFromInteger 0) [1 .. 100000]
  where
    go :: ParityInteger -> [Integer] -> ParityInteger
    go acc [] = acc
    go acc (x : xs) = go (add acc (piFromInteger x)) xs

type ParityInteger# :: TYPE (TupleRep [LiftedRep, LiftedRep])
newtype ParityInteger# = MkPi# (# Parity, Integer #)

-- >>> :set -fprint-explicit-runtime-reps
-- >>> :kind (# , #)
-- (# , #) :: TYPE k0 -> TYPE k1 -> TYPE ('TupleRep '[k0, k1])

-- >>> :set -fprint-explicit-runtime-reps
-- >>> :t ($)
-- ($)
--   :: forall (repa :: RuntimeRep) (repb :: RuntimeRep)
--             (a :: TYPE repa) (b :: TYPE repb).
--      (a -> b) -> a -> b

add# :: ParityInteger# -> ParityInteger# -> ParityInteger#
add# (MkPi# (# p1, n1 #)) (MkPi# (# p2, n2 #)) = MkPi# (# p1 + p2, n1 + n2 #)

piFromInteger# :: Integer -> ParityInteger#
piFromInteger# n
  | even n = MkPi# (# Even, n #)
  | otherwise = MkPi# (# Odd, n #)

integerFromPi# :: ParityInteger# -> Integer
integerFromPi# (MkPi# (# _, n #)) = n

bigPI# :: () -> ParityInteger#
bigPI# () = go (lpFromInteger 0) [1 .. 100000]
  where
    -- go :: ParityInteger# -> [Integer] -> ParityInteger#                                       -- pass
    go :: forall (a :: TYPE (TupleRep [LiftedRep, LiftedRep])). (LPNum a) => a -> [Integer] -> a -- pass
    -- go :: forall (r :: RuntimeRep) (a :: TYPE r). LPNum a => a -> [Integer] -> a              -- error
    -- we can never bind a variable to a type that does not have a fixed runtime representation
    go acc [] = acc
    go acc (x : xs) = go (lpPlus acc (lpFromInteger x)) xs

class LPNum (a :: TYPE r) where
  lpPlus :: a -> a -> a
  lpFromInteger :: Integer -> a
  lpToInteger :: a -> Integer

-- here a :: TYPE r, why we can bind variable to a in the instance declaration?
-- because when we write the instance, we already know the "Type" of a

instance LPNum Integer where
  -- like here we know that a :: TYPE LiftedRep
  lpPlus = (+)
  lpFromInteger = fromInteger
  lpToInteger = id

instance LPNum ParityInteger# where
  lpPlus = add#
  lpFromInteger = piFromInteger#
  lpToInteger = integerFromPi#

instance LPNum ParityInteger where
  lpPlus = add
  lpFromInteger = piFromInteger
  lpToInteger (MkPi _ n) = n

main :: IO ()
main = print (lpToInteger (bigPI# ()))
