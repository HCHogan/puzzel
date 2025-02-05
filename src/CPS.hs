module CPS (pythagoras_cps) where

import Control.Monad
import Control.Monad.Cont

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (x + y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (x * x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
  square_cps x $ \x_squared ->
    square_cps y $ \y_squared ->
      add_cps x_squared y_squared $ k

-- >>> pythagoras_cps 3 4 print

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
  f_cps x $ \fx ->
    f_cps fx $ \ffx ->
      f_cps ffx $ k

-- Having continuation-passing functions, the next step is providing a neat way of composing them, preferably one which does not require the long chains of nested lambdas we have seen just above. A good start would be a combinator for applying a CPS function to a suspended computation. A possible type for it would be:
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS x f_cps = \k ->
  x $ \fx ->
    f_cps fx $ k

-- Doesn't the type of chainCPS look familiar? If we replace (a -> r) -> r with (Monad m) => m a and (b -> r) -> r with (Monad m) => m b we get the (>>=) signature. Furthermore, our old friend flip ($) plays a return-like role, in that it makes a suspended computation out of a value in a trivial way. Lo and behold, we have a monad! All we need now [3] is a Cont r a type to wrap suspended computations, with the usual wrapper and unwrapper functions.
data MyCont r a = MyCont {runMyCont :: ((a -> r) -> r)}

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
      runMyCont (f x) $ c

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
