{-# LANGUAGE InstanceSigs #-}

module Com () where

import Control.Comonad

type Coord = (Int, Int)

data Grid a = Grid (Coord -> a) Coord

move :: (Int, Int) -> Grid a -> Grid a
move (dx, dy) (Grid f (x, y)) = Grid f (x + dx, y + dy)

moveTo :: (Int, Int) -> Grid a -> Grid a
moveTo p (Grid f _) = Grid f p

instance Functor Grid where
  fmap g (Grid f pos) = Grid (g . f) pos

-- I like to think of a Store as a big warehouse filled with values of type a. Each value of type a is slotted into a position labeled by an index value of type s. Finally there is a forklift parked at position pos. The forklift can be used to extract a value of type a from the store by pulling the value out from where it is parked. You can use seek to move the forklift to a new absolute position or use seeks to move the forklift to a new relative location. To update all values of the store use fmap.
-- extend f is similar to fmap except instead of f :: a -> a' we have f :: Store s a -> a' which lets the update function not only have access to the value being updated but also gives access to the value's position and access to the values of everything else in the store. In other words, extend uses the value plus its surrounding context to perform the update.
instance Comonad Grid where
  extract :: Grid a -> a
  extract (Grid f pos) = f pos

  duplicate :: Grid a -> Grid (Grid a)
  duplicate g@(Grid f pos) = Grid (`moveTo` g) pos

neighbors :: Grid a -> [a]
neighbors grid = [extract (move d grid) | d <- deltas]
 where
  deltas =
    [ (-1, -1)
    , (-1, 0)
    , (-1, 1)
    , (0, -1)
    , (0, 1)
    , (1, -1)
    , (1, 0)
    , (1, 1)
    ]

smooth :: Grid Double -> Double
smooth grid = (center + sum nbrs) / (1 + fromIntegral (length nbrs))
 where
  center = extract grid
  nbrs = neighbors grid

gridFromFunction :: (Coord -> a) -> Coord -> Grid a
gridFromFunction = Grid

exampleGrid :: Grid Double
exampleGrid = gridFromFunction (\(x, y) -> fromIntegral (x + y)) (0, 0)

smoothedGrid :: Grid Double
smoothedGrid = extend smooth exampleGrid

displayGrid :: Int -> Grid Double -> IO ()
displayGrid n grid = mapM_ putStrLn rows
 where
  (x0, y0) = case grid of Grid _ pos -> pos
  coords = [(x, y) | y <- [y0 - n .. y0 + n], x <- [x0 - n .. x0 + n]]
  rows =
    [ unwords
        [ show (extract (move (dx, dy) grid))
        | dx <- [-n .. n]
        , let dy = r - y0
        ]
    | r <- [y0 - n .. y0 + n]
    ]
