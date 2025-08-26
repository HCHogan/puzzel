module Comonad where

import Control.Comonad

data ListZipper a = LZ [a] a [a] deriving (Show, Eq, Functor)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate z = LZ (lefts z) z (rights z)
    where
      lefts = tail . iterate goLeft
      rights = tail . iterate goRight

      goLeft (LZ (l : ls) x rs) = LZ ls l (x : rs)
      goLeft lz = lz -- stop at border
      goRight (LZ ls x (r : rs)) = LZ (x : ls) r rs
      goRight lz = lz

myZipper :: ListZipper Int
myZipper = LZ [2, 1] 3 [4, 5]

sumNeighbors :: ListZipper Int -> Int
sumNeighbors (LZ (l : _) x (r : _)) = l + x + r
sumNeighbors (LZ [] x (r : _)) = x + r
sumNeighbors (LZ (l : _) x []) = x + l
sumNeighbors (LZ [] x []) = x

result :: ListZipper Int
result = extend sumNeighbors myZipper

-- >>> result
