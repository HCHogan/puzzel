{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use camelCase" -}

module CPS (pythagoras_cps, try2Cont, example) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y k = k (x + y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x k = k (x * x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y k = square_cps x $ \x_squared ->
  square_cps y $ \y_squared ->
    add_cps x_squared y_squared k

pythagoras_cps' :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps' x y k =
  square_cps
    x
    ( \x_squared ->
        square_cps
          y
          ( \y_squared ->
              add_cps x_squared y_squared k
          )
    )

-- >>> pythagoras_cps 3 4 id
-- 25

-- >>> pythagoras_cps' 3 4 id
-- 25

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x k = f_cps x $ \fx ->
  f_cps fx $ \ffx ->
    f_cps ffx k

-- Having continuation-passing functions, the next step is providing a neat way of composing them, preferably one which does not require the long chains of nested lambdas we have seen just above. A good start would be a combinator for applying a CPS function to a suspended computation. A possible type for it would be:
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS x f_cps k = x $ \fx ->
  f_cps fx k

-- Doesn't the type of chainCPS look familiar? If we replace (a -> r) -> r with (Monad m) => m a and (b -> r) -> r with (Monad m) => m b we get the (>>=) signature. Furthermore, our old friend flip ($) plays a return-like role, in that it makes a suspended computation out of a value in a trivial way. Lo and behold, we have a monad! All we need now [3] is a Cont r a type to wrap suspended computations, with the usual wrapper and unwrapper functions.
newtype MyCont r a = MyCont {runMyCont :: (a -> r) -> r}

myCont :: ((a -> r) -> r) -> MyCont r a
myCont = MyCont

instance Functor (MyCont r) where
  fmap f (MyCont x) = MyCont $ \k -> x (k . f)

instance Applicative (MyCont r) where
  pure x = MyCont ($ x)
  (MyCont f) <*> (MyCont x) = MyCont $ \k ->
    f $ \ff ->
      x $ \xx ->
        k (ff xx)

-- The monad instance for Cont follows directly from our presentation, the only difference being the wrapping and unwrapping cruft:
instance Monad (MyCont r) where
  (MyCont s) >>= f = MyCont $ \c ->
    s $ \x ->
      runMyCont (f x) c

add_cont :: Int -> Int -> MyCont r Int
add_cont x y = MyCont $ \k -> k (x + y)

square_cont :: Int -> MyCont r Int
square_cont x = MyCont $ \k -> k (x * x)

pythagoras_cont :: Int -> Int -> MyCont r Int
pythagoras_cont x y = do
  x_squared <- square_cont x
  y_squared <- square_cont y
  add_cont x y

-- callCC, a function which gives us back explicit control of continuations - but only where we want it.
-- Without callCC
square :: Int -> Cont r Int
square n = return (n * n)

-- With callCC
-- squareCCC
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n * n)

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c : s
    when (s0 == "hello") $ k "they say hello"
    let s1 = show s0
    return ("They appear to be saying " ++ s1)
  return (length msg)

-- >>> runCont (bar 'h' "ello") id
-- 14

quux :: Int -> Cont r Int
quux n = callCC $ \k -> do
  when (n < 5) $ k 1
  return 0

-- >>> runCont (quux 3) id
-- 1

-- >>> runCont (quux 7) id
-- 0

-- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
-- We can make sense of that based on what we already know about callCC. The overall result type and the result type of the argument have to be the same (i.e. Cont r a), as in the absence of an invocation of k the corresponding result values are one and the same. Now, what about the type of k? As mentioned above, k's argument is made into a suspended computation inserted at the point of the callCC invocation; therefore, if the latter has type Cont r a k's argument must have type a. As for k's result type, interestingly enough it doesn't matter as long as it is wrapped in the same Cont r monad; in other words, the b stands for an arbitrary type. That happens because the suspended computation made out of the a argument will receive whatever continuation follows the callCC, and so the continuation taken by k's result is irrelevant.

-- The arbitrariness of k's result type explains why the following variant of the useless line example leads to a type error:
-- quux :: Cont r Int
-- quux = callCC $ \k -> do
--    let n = 5
--    when True $ k n
--    k 25
-- k's result type could be anything of form Cont r b; however, the when constrains it to Cont r (), and so the closing k 25 does not match the result type of quux. The solution is very simple: replace the final k by a plain old return.

-- callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
-- when f in callCC calls the escape function, the result of `f (\a -> cont $ \_ -> h a)` becomes *cont $ (\_ -> h a)*
-- which will be called with k, then becomes h a

-- myCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
-- myCallCC f = cont $ \k -> runContT (f (\a -> cont $ \_ -> k a)) k

fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \k1 -> do
    when (n < 0) $ k1 (show n)
    let ns = map digitToInt (show (n `div` 2))
    n' <- callCC $ \k2 -> do
      when (length ns < 3) $ k2 (length ns)
      when (length ns < 5) $ k2 n
      when (length ns < 7) $ do
        let ns' = map intToDigit (reverse ns)
        k1 (dropWhile (== '0') ns') -- escape 2 levels
      return $ sum ns
    return $ "(ns = " ++ show ns ++ ") " ++ show n'
  return $ "Answer: " ++ str

fun2 :: Int -> String
fun2 n = (`runCont` id) $ callCC $ \k -> do
  let _ = k (show 1)
  return $ show n

fun3 :: Int -> String
fun3 n = (`runCont` id) $ do
  str <- callCC $ \k -> do
    when (n < 0) $ k "negative"
    return $ show n
  return $ "Answer: " ++ str

-- >>> fun3 123
-- "Answer: 123"

-- >>> fun3 (-123)
-- "Answer: negative"

-- >>> fun2 123
-- "123"

-- k controls the control flow by ignoring the rest of the continuations

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    when (y == 0) $ notok "Div 0"
    ok $ x `div` y
  handler err

-- >>> runCont (divExcpt 10 0 (return . length)) id
-- 5

-- >>> runCont (divExcpt 10 5 (return . length)) id
-- 2

tryCont :: (MonadCont m) => ((t -> m b) -> m a) -> (t -> m a) -> m a
tryCont c h = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    x <- c notok
    ok x
  h err

try2Cont :: IO ()
try2Cont = evalContT $ do
  gotoA <- callCC $ \k -> let go = k go in pure go
  pwd <- liftIO $ do
    putStr "enter your password"
    getLine
  when (pwd /= "123") gotoA
  liftIO $ putStrLn "finished"

try3Cont :: Int -> (String -> Cont r String) -> Cont r String
try3Cont n handler = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    when (n < 0) $ notok "negative"
    ok $ show n
  handler err

fun4 :: (MonadCont m) => Int -> m String
fun4 n = callCC $ \k -> do
  when (n < 0) $ k ""
  return $ show n

-- applicative do
foo5 ma mb = do
  a <- ma
  b <- mb
  return (a, b)

data SqrtException = LessThanZero deriving (Show, Eq)
sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  ln <- liftIO (putStr "ENter a number to sqrt: " *> readLn)
  when (ln < 0) (throw LessThanZero)
  liftIO $ print (sqrt ln)

-- In this section we make a CoroutineT monad that provides a monad with fork, which enqueues a new suspended coroutine, and yield, that suspends the current thread.

-- The CoroutineT monad is just ContT stacked with a StateT containing the suspended coroutines.
newtype CoroutineT r m a = CoroutineT {runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a}
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- used to manipulate the coroutine queue
getCCs :: (Monad m) => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: (Monad m) => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

-- pop and push coroutines to the queue.
dequeue :: (Monad m) => CoroutineT r m ()
dequeue = do
  current_ccs <- getCCs
  case current_ccs of
    [] -> return ()
    (p : ps) -> do
      putCCs ps
      p

queue :: (Monad m) => CoroutineT r m () -> CoroutineT r m ()
queue p = do
  cc <- getCCs
  putCCs (cc ++ [p])

-- The interface
yield :: (Monad m) => CoroutineT r m ()
yield = callCC $ \k -> do
  queue (k ())
  dequeue

fork :: (Monad m) => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> do
  queue (k ())
  p
  dequeue

exhaust :: (Monad m) => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> getCCs
  unless exhausted $ yield >> exhaust

-- Runs the coroutines in the base monad.
runCoroutineT :: (Monad m) => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

printOne n = do
  liftIO (print n)
  yield

example :: (MonadIO m) => m ()
example = runCoroutineT $ do
  fork $ replicateM_ 3 (printOne 3)
  fork $ replicateM_ 4 (printOne 4)
  replicateM_ 2 (printOne 2)
