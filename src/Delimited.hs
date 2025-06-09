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

assert :: (HasCallStack) => Bool -> IO ()
assert True = print "Assertion passed"
assert False = error "Assertion failed"

newtype Exception e a = Throw e

-- `control0` uses the tag to lookup for the matching `catch` in the callstack
throw :: Exception e % r -> e -> Mom a
throw tag e = control0 tag \_ -> pure (Op (Throw e))

-- f accpets a tag - so that it may call the throw. In catch f onThrow, a tag was created, passed to f
-- the f tag either returns nomarlly with value wrapped in `Pure`, or throws an exception wrapped in `Op . Throw`
-- More generally, for an effect expressed as a functor f, things will be set up exactly so that
-- handlers will be matching on a computation/tree of type Free f r.
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

-- Output
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
  handle tag $ Pure <$> f tag
  where
    handle tag action = do
      prompt tag action >>= \case
        Op (Output o continue) -> pure (o : runList (handle tag $ continue $ pure ()))
        Pure () -> pure []
    runList = fromMaybe [] . run

discardedCollect :: (Out o % a -> Mom a) -> Mom a
discardedCollect f = do
  tag <- newPromptTag
  handle tag $ Pure <$> f tag
  where
    handle tag action = do
      prompt tag action >>= \case
        Op (Output _ continue) -> handle tag $ continue $ pure ()
        Pure a -> pure a

tracedCatch :: Out String % r -> Mom Bool
tracedCatch out = catch this onThrow
  where
    this exc = do
      log out "Start"
      void $ throw exc "Boom"
      log out "Unreachable"
      pure False
    onThrow e = do
      log out $ "Error:" ++ e
      pure True

-- Input
newtype In i a = Input (Mom i -> Mom a)

input :: In i % r -> Mom i
input tag = control0 tag $ pure . Op . Input

csum :: In Int % r -> Out Int % r -> Mom a
csum inp out = go 0
  where
    go !acc = do
      n <- input inp
      let acc' = acc + n
      output out acc'
      go acc'

-- call input with provided list
listInput :: [i] -> (In i % a -> Mom a) -> Mom (Maybe a)
listInput l f = do
  tag <- newPromptTag
  catch (\exc -> handle exc tag l (Pure <$> f tag)) (const $ pure Nothing)
  where
    handle exc tag l action = do
      prompt tag action >>= \case
        Op (Input continue)
          | x : xs <- l -> handle exc tag xs $ continue $ pure x
          | otherwise -> handle exc tag [] $ continue $ throw exc ()
        Pure a -> pure $ Just a

testFib :: IO ()
testFib =
  assert $
    take 8 (collect fibonacci) == [0, 1, 1, 2, 3, 5, 8, 13]

testThrow :: IO ()
testThrow = do
  assert (isRight' $ run $ try $ const $ pure "Result")
  assert (isLeft' $ run $ try (`throw` "Error"))
  where
    isRight' = maybe False isRight
    isLeft' = maybe False isLeft

testTracedCatch :: IO ()
testTracedCatch = do
  assert (collect (void . tracedCatch) == ["Start", "Error:Boom"])

testCsum :: IO ()
testCsum =
  assert
    ( ( collect \out ->
          void $ listInput [1 .. 5] (`csum` out)
      )
        == [1, 3, 6, 10, 15]
    )
