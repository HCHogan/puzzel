module Traversable () where

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative

numbers = [1, 2, 3]

