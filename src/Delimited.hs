{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Delimited where

import Control.Exception qualified as E
import Control.Exception.Base (NoMatchingContinuationPrompt (..))
import Data.Either
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Sum (Sum (..))
import Data.Maybe (fromMaybe, maybe)
import GHC.Exts (PromptTag#, control0#, newPromptTag#, prompt#)
import GHC.IO (IO (..))
import GHC.Stack (HasCallStack)
import System.Environment
import System.IO.Unsafe
import Prelude hiding (log)

newtype Mom a = Mom (IO a)
  deriving (Functor, Applicative, Monad)

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: Mom (PromptTag a)
newPromptTag = Mom $ IO $ \s ->
  case newPromptTag# s of
    (# s', tag #) -> (# s', PromptTag tag #)

-- | Church-encoded natural numbers
newtype Nat = Nat (forall a. a -> (a -> a) -> a)

-- | zero = λz f. z
mkZero :: Nat
mkZero = Nat const

-- | succ n = λz f. f (n z f)
mkSucc :: Nat -> Nat
mkSucc (Nat n) = Nat $ \z f -> f (n z f)

-- | elimNat z f n = n z f
elimNat :: a -> (a -> a) -> Nat -> a
elimNat z f (Nat n) = n z f

-- | add x y = λz f. x (y z f) f
addNat :: Nat -> Nat -> Nat
addNat x y = elimNat y mkSucc x

toInt :: Nat -> Int
toInt (Nat n) = n 0 (+ 1)

-- 示例
two, three, five :: Nat
two = mkSucc (mkSucc mkZero)
three = mkSucc two
five = addNat two three

-- >>> toInt five
-- 5

