module Free where

data Eff f a where
  Return :: a -> Eff f a
  Then :: f a -> (a -> Eff f b) -> Eff f b

data State s a where
  Get :: State s s
  Put :: s -> State s ()

runState :: s -> Eff (State s) a -> (s, a)
runState s (Return a) = (s, a)
runState s (Get `Then` k) = runState s (k s)
runState _ (Put s `Then` k) = runState s (k ())

program :: Eff (State Int) Int
program =
  Get `Then` \n ->
    if n <= 0
      then Return n
      else Put (n - 1) `Then` const program
