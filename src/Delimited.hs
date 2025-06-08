{-# LANGUAGE BlockArguments #-}
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

type f % r = PromptTag (Free f r)

data Free f r
  = Op (f (Free f r))
  | Pure r

-- | https://hackage.haskell.org/package/ghc-prim-0.13.0/docs/GHC-Prim.html#continuations
newtype Mom a = Mom (IO a)
  deriving (Functor, Applicative, Monad)

-- Just some thin wrapper around GHC's primitives
data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: Mom (PromptTag a)
newPromptTag = Mom $ IO $ \s ->
  case newPromptTag# s of
    (# s', tag #) -> (# s', PromptTag tag #)

prompt :: PromptTag a -> Mom a -> Mom a
prompt (PromptTag tag) (Mom (IO a)) = Mom $ IO $ prompt# tag a

control0 :: PromptTag a -> ((Mom b -> Mom a) -> Mom a) -> Mom b
control0 (PromptTag tag) f =
  Mom $
    IO $
      control0#
        tag
        ( \k -> case f (\(Mom (IO a)) -> Mom (IO (k a))) of
            Mom (IO b) -> b
        )

-- Look Ma', no IO!
run :: Mom a -> Maybe a
run (Mom m) =
  unsafePerformIO
    (E.catch (Just <$> m) \NoMatchingContinuationPrompt -> pure Nothing)

newtype Exception e a = Throw e

throw :: Exception e % r -> e -> Mom a
throw tag e = control0 tag \_ -> pure (Op (Throw e))

catch :: (Exception e % a -> Mom a) -> (e -> Mom a) -> Mom a
catch f onThrow = do
  tag <- newPromptTag
  handle tag $ f tag
  where
    handle tag action = do
      next <- prompt tag (Pure <$> action)
      case next of
        Op (Throw e) -> onThrow e
        Pure a -> pure a

-- the explicit tags serve as a form of capabilities, handles that functions take as explicit arguments
-- granting the permission to use the associated effects
try :: (Exception e % Either e a -> Mom a) -> Mom (Either e a)
try f = catch (fmap Right . f) (pure . Left)

testThrow :: IO ()
testThrow = do
  assert (isRight' $ run $ try $ const $ pure "Result")
  assert (isLeft' $ run $ try (`throw` "Error"))
  where
    isRight' = maybe False isRight
    isLeft' = maybe False isLeft

data Out o a = Output o (Mom () -> Mom a)

output :: Out o % r -> o -> Mom ()
output tag o = control0 tag $ pure . Op . Output o

log :: Out String % r -> String -> Mom ()
log = output

fibonacci :: Out Int % r -> Mom a
fibonacci out = fib 0 1
  where
    fib !a !b = do
      output out a
      fib b (a + b)

collect :: (Out o % () -> Mom ()) -> [o]
collect f = runList do
  tag <- newPromptTag
  handle tag (Pure <$> f tag)
  where
    handle tag action = do
      next <- prompt tag action
      case next of
        Op (Output o continue) -> pure (o : runList (handle tag $ continue $ pure ()))
        Pure () -> pure []
    runList = fromMaybe [] . run

testFib :: IO ()
testFib =
  assert $
    take 8 (collect fibonacci) == [0, 1, 1, 2, 3, 5, 8, 13]

assert :: (HasCallStack) => Bool -> IO ()
assert True = print "Assertion passed"
assert False = error "Assertion failed"
