module Free where

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

-- first >>= lifts f :: a -> Free f b to Free f a -> Free f b, and the <$> lifts it to f (Free f a) -> f (Free f b), the implementation is very similar to the one for Functor

newtype StateF s a = StateF { runStateF :: s -> (a, s) }
  deriving stock Functor

getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

liftF :: Functor f => f a -> Free f a
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
  let (m, s') = runStateF f s in
  runState m s'

