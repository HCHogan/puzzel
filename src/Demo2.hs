{-# LANGUAGE Arrows #-}

module Demo2 where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import Data.Tuple
import System.Random

newtype Circuit a b = Circuit {unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id = Circuit (Cat.id,)
  (.) (Circuit cir2) (Circuit cir1) = Circuit $ \x ->
    let (cir1', y) = cir1 x
        (cir2', z) = cir2 y
     in (cir2' Cat.. cir1', z)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)
  first (Circuit cir) = Circuit $ \(a, b) ->
    let (cir', c) = cir a
     in (first cir', (c, b))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit (Circuit cir) (x : xs) =
  let (cir', y) = cir x
   in y : runCircuit cir' xs

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
  let (b, acc') = f input acc
   in (accum acc' f, b)

accum' :: acc -> (a -> acc -> acc) -> Circuit a acc
accum' acc f = accum acc (\a b -> let b' = f a b in (b', b'))

total :: (Num a) => Circuit a a
total = accum' 0 (+)

-- >>> runCircuit total [1, 2, 3]
-- [1,3,6]

mean1 :: (Fractional a) => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

-- >>> runCircuit mean1 [1, 2, 3]
-- [1.0,1.5,2.0]

generator :: (Random a) => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \() rng -> randomR range rng

dictionary :: [String]
dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
  idx <- generator (0, length dictionary - 1) rng -< ()
  returnA -< dictionary !! idx

-- >>> rng <- getStdGen
-- >>> runCircuit (pickWord rng) $ replicate 3 ()
-- ["bird","dog","bird"]

oneShot :: Circuit () Bool
oneShot = accum True $ \() acc -> (acc, False)

-- >>> runCircuit oneShot $ replicate 6 ()
-- [True,False,False,False,False,False]

delayedEcho :: a -> Circuit a a
delayedEcho acc = accum acc $ flip (,)

instance ArrowChoice Circuit where
  left orig@(Circuit cir) = Circuit $ \case
    Left b -> let (cir', c) = cir b in (left cir', Left c)
    Right d -> (left orig, Right d)

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
  firstTime <- oneShot -< ()
  mPicked <-
    if firstTime -- we can use `if` if `ArrowChoice` is implemented
      then do
        picked <- pickWord rng -< ()
        returnA -< Just picked
      else returnA -< Nothing
  mWord <- accum' Nothing mplus -< mPicked
  returnA -< fromJust mWord

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung =
  "Lives: ["
    ++ replicate (attempts - hung) '#'
    ++ replicate hung ' '
    ++ "]"

mySecond :: (Arrow y) => y a b -> y (c, a) (c, b)
mySecond yab = arr swap >>> first yab >>> arr swap

myRight :: (ArrowChoice y) => y a b -> y (Either c a) (Either c b)
myRight a = arr (\case Left x -> Right x; Right x -> Left x) >>> left a >>> arr (\case Left x -> Right x; Right x -> Left x)

liftY2 :: (Arrow y) => (a -> b -> c) -> y r a -> y r b -> y r c
liftY2 f yra yrb = (yra &&& yrb) >>> arr (uncurry f)
