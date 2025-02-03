module Playground () where

class Collects s where
  insert :: s a -> a -> s a

instance Collects [] where
  insert xs x = x : xs
