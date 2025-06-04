{-# LANGUAGE RequiredTypeArguments #-}
module Free where

import Data.Functor
import Data.Kind

data Eff f a where
  Return :: a -> Eff f a
  Then :: f a -> (a -> Eff f b) -> Eff f b

data StateE s a where
  Get :: StateE s s
  Put :: s -> StateE s ()

runStateE :: s -> Eff (StateE s) a -> (s, a)
runStateE s (Return a) = (s, a)
runStateE s (Get `Then` k) = runStateE s (k s)
runStateE _ (Put s `Then` k) = runStateE s (k ())

program :: Eff (StateE Int) Int
program =
  Get `Then` \n ->
    if n <= 0
      then Return n
      else Put (n - 1) `Then` const program

-- We can guess a few things based on the intuition we gained by looking at free monoids. First, we might note that return and >=> correspond to mempty and (<>), respectively. Second, we can expect that for any Functor f :: Type -> Type, there is a corresponding free Monad Free f :: Type -> Type, the same as for free monoids.

-- Let us first try to construct `Free f` by analogy
-- data List a = Nil | Cons a (List a)
-- data Free f a = Pure a | Free (f (Free f a))

data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a

-- Free here is an embedding from Functor :: Type -> Type to Monad :: Type -> Type
-- notice that Pure has the same singature as Return, Free has a very similar signature of join:
-- join :: Monad m => m (m a) -> m a

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free g) = Free (fmap f <$> g)

-- first fmap lifts f :: a -> b to Free f a -> Free f b, second fmap lifts it to f (Free f a) -> f (Free f b), and that's why we need f to be a Functor to recursively apply it to the inner structure.

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Free gfn <*> x = Free (fmap (<*> x) gfn)

instance (Functor f) => Monad (Free f) where
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

-- first (>>=) lifts f :: a -> Free f b to Free f a -> Free f b, and the <$> lifts it to f (Free f a) -> f (Free f b), the implementation is very similar to the one for Functor

newtype StateF s a = StateF {runStateF :: s -> (a, s)}
  deriving stock (Functor)

getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

liftF :: (Functor f) => f a -> Free f a
liftF fa = Free $ Pure <$> fa

get :: State s s
get = liftF getF

put :: s -> State s ()
put = liftF . putF

something :: State Int ()
something = do
  i <- get
  put $ i + 1
  pure ()

runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
   in runState m s'

-- we've essentially moved the implementation of implementation of `>>=` to `runState`. The free monad doesn’t specify the meaning of monadic actions, so we have to decide what those actions mean when we’re running it. To illustrate this, we can implement write another interpreter, which is essentially a pretty-printer for the flow of a State computation.

printState :: (Show s, Show a) => State s a -> s -> String
printState (Pure x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Free m) s =
  let (x, s') = runStateF m s
   in "state change" <> show s <> " -> " <> show s' <> " | " <> printState x s'

-- >>> printState something 1
-- "state change1 -> 1 | state change1 -> 2 | pure ((),2)"

-- In general, we can restore any proper monad behavior from a free monad since it dosen't define any sementics beyond those necessary for any monad. free provides a function for that:

foldFree :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = return a
foldFree f (Free g) = do
  x <- f g
  foldFree f x

-- lets define a eDSL using free monads:

data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving (Functor)

type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()

astProgram :: (Read a, Show a) => FreeAST a ()
astProgram = do
  x <- input
  y <- input
  res <- add x y
  output res

computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
    go :: ASTF Int a -> IO a
    go = \case
      Add x y next -> pure $ next (x + y)
      Input next -> next . read <$> getLine
      Output x next -> print x $> next

-- Trees as free monads
data BinTreeF l a = NodeF l a a
  deriving (Functor)

type FreeBinTree l = Free (BinTreeF l)

buildBalanced :: [Int] -> FreeBinTree Int (Maybe Int)
buildBalanced [] = pure Nothing
buildBalanced [x] = pure $ Just x
buildBalanced xs = do
  let len = length xs
  let (l, x:r) = splitAt (len `div` 2) xs 
  b <- liftF $ NodeF x l r
  buildBalanced b
  
data A = A1 | A2

type family B (x :: A) :: Type where
  B 'A1 = Int
  B 'A2 = Bool

-- fff :: forall (x :: A) -> B x
-- fff A1 = 42
-- fff A2 = True

idVdq :: forall (a :: Type) -> a -> a
idVdq t x = x

readshow :: forall a -> (Read a, Show a) => String -> String
readshow t s = show (read s :: t)

-- >>> readshow Int "42"
-- "42"

-- >>> readshow Double "3.14"
-- "3.14"

-- >>> idVdq Int 123
-- 123

