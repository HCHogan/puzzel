module Exists where

data Counter = forall a. Counter {new :: a, inc :: a -> a, get :: a -> Int}

test :: IO ()
test = do
  let a = Counter {new = 0, inc = (+ 1), get = id}
      b = Counter {new = "", inc = (++ "!"), get = length}
  case a of
    Counter {new = n, inc = i, get = g} -> do
      print $ g (i n)
  case b of
    Counter {new = n, inc = i, get = g} -> do
      print $ g (i n)

  -- let c = new a
  return ()
