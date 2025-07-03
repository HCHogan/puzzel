{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Delimited where

import Control.Exception qualified as E
import Control.Exception.Base (NoMatchingContinuationPrompt (..))
import Data.Either
import Data.Foldable (for_)
import Data.Functor (void, ($>))
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
      prompt tag (Pure <$> action) >>= \case
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

-- Feed the output of one computation into the input of the other. Terminate whenever one side terminates, discarding the other.
connect :: (Out x % a -> Mom a) -> (In x % a -> Mom a) -> Mom a
connect producer consumer = do
  out <- newPromptTag
  inp <- newPromptTag
  handleI out inp (Pure <$> producer out) (Pure <$> consumer inp)
  where
    handleI out inp produce consume = do
      prompt inp consume >>= \case
        Op (Input continue) -> handleO out inp produce continue
        Pure a -> pure a
    handleO out inp produce consume = do
      prompt out produce >>= \case
        Op (Output o continue) -> handleI out inp (continue $ pure ()) (consume $ pure o)
        Pure a -> pure a

-- Text output can be printed to stdout
printOutput :: (Out String % () -> Mom ()) -> IO ()
printOutput f = momToIO do
  tag <- newPromptTag
  handle tag (Pure <$> f tag)
  where
    handle out action = do
      prompt out action >>= \case
        Op (Output o continue) -> pure $ print o *> momToIO (handle out (continue $ pure ()))
        Pure () -> pure $ pure ()
    momToIO = fromMaybe (pure ()) . run

-- we can forward text input from stdin
readInput :: (In String % () -> Mom ()) -> IO ()
readInput f = momToIO do
  tag <- newPromptTag
  handle tag (Pure <$> f tag)
  where
    handle inp action = do
      prompt inp action >>= \case
        Op (Input continue) -> pure do
          i <- getLine
          momToIO (handle inp (continue $ pure i))
    momToIO = fromMaybe (pure ()) . run

-- State
data State s a
  = Get (Mom s -> Mom a)
  | Put s (Mom () -> Mom a)

get :: State s % r -> Mom s
get tag = control0 tag $ pure . Op . Get

put :: State s % r -> s -> Mom ()
put tag s = control0 tag $ pure . Op . Put s

runState :: s -> (State s % a -> Mom a) -> Mom (s, a)
runState s0 f = do
  tag <- newPromptTag
  handle tag s0 (Pure <$> f tag)
  where
    handle tag s action = do
      prompt tag action >>= \case
        Op (Get continue) -> handle tag s (continue $ pure s)
        Op (Put s continue) -> handle tag s (continue $ pure ())
        Pure a -> pure (s, a)

incr :: State Int % r -> Mom ()
incr st = do
  n <- get st
  put st $ n + 1

logState :: Out String % r -> State Int % s -> Mom ()
logState out st = do
  n <- get st
  log out $ show n

incr2 :: Out String % r -> State Int % s -> Mom ()
incr2 out st = do
  incr st
  logState out st
  incr st
  logState out st

-- Nondeterminism
data Nondet a where
  Choose :: [x] -> (Mom x -> Mom a) -> Nondet a

choose :: Nondet % r -> [x] -> Mom x
choose tag xs = control0 tag $ pure . Op . Choose xs

nameTheorems :: Nondet % r -> Mom String
nameTheorems nd = do
  name1 <- choose nd ["Church", "Curry"]
  name2 <- choose nd ["Turing", "Howard"]
  result <- choose nd ["thesis", "isomorphism"]
  pure (name1 ++ "-" ++ name2 ++ " " ++ result)

enumerate :: (Nondet % a -> Mom a) -> Out a % r -> Mom ()
enumerate f out = do
  tag <- newPromptTag
  handle tag (Pure <$> f tag)
  where
    handle tag action = do
      next <- prompt tag action
      case next of
        Op (Choose xs continue) -> for_ xs (handle tag . continue . pure)
        Pure a -> output out a

-- Concurrency
data Conc a
  = Fork (Mom ()) (Mom () -> Mom a)
  | Yield (Mom () -> Mom a)

fork :: Conc % r -> Mom () -> Mom ()
fork tag thread = control0 tag $ pure . Op . Fork thread

yield :: Conc % r -> Mom ()
yield tag = control0 tag $ pure . Op . Yield

runConc :: (Conc % () -> Mom ()) -> Mom ()
runConc f = do
  tag <- newPromptTag
  handle tag [Pure <$> f tag]
  where
    handle tag [] = pure ()
    handle tag (t : ts) = do
      prompt tag t >>= \case
        Op (Yield continue) -> handle tag (ts ++ [continue $ pure ()])
        Op (Fork th continue) -> handle tag (continue (pure ()) : ts ++ [Pure <$> th])
        Pure () -> handle tag ts

simpleThread :: Out String % r -> Conc % s -> Int -> Mom ()
simpleThread out conc n = do
  log out $ show n
  yield conc
  log out $ show n
  yield conc
  log out $ show n
  yield conc

-- tests
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

testState :: IO ()
testState = do
  assert ((collect \out -> runState 0 (incr2 out) $> ()) == ["1", "2"])
  assert (run (discardedCollect $ runState 0 . incr2) == Just (2, ()))

testEnumerate :: IO ()
testEnumerate = do
  assert
    ( collect (enumerate nameTheorems)
        == [ "Church-Turing thesis",
             "Church-Turing isomorphism",
             "Church-Howard thesis",
             "Church-Howard isomorphism",
             "Curry-Turing thesis",
             "Curry-Turing isomorphism",
             "Curry-Howard thesis",
             "Curry-Howard isomorphism"
           ]
    )

testInterleave :: IO ()
testInterleave =
  assert
    ( collect (runConc . interleave123)
        == ["1", "2", "3", "1", "2", "3", "1", "2", "3"]
    )
  where
    interleave123 :: Out String % r -> Conc % s -> Mom ()
    interleave123 out conc = do
      fork conc (simpleThread out conc 1)
      fork conc (simpleThread out conc 2)
      fork conc (simpleThread out conc 3)
