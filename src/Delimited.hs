{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Delimited where

import qualified Control.Exception as E
import Control.Exception.Base (NoMatchingContinuationPrompt(..))
import Data.Either
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Functor.Sum (Sum(..))
import Data.Maybe (fromMaybe, maybe)
import System.IO.Unsafe
import System.Environment
import GHC.Exts (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO(..))
import GHC.Stack (HasCallStack)
import Prelude hiding (log)

newtype Mom a = Mom (IO a)
  deriving (Functor, Applicative, Monad)

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: Mom (PromptTag a)
newPromptTag = Mom $ IO $ \s ->
  case newPromptTag# s of
    (# s', tag #) -> (# s', PromptTag tag #)

