{-# LANGUAGE DefaultSignatures #-}

module Traversable (transpose) where

import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative

numbers = [1, 2, 3]

class (Functor t, Foldable t) => MyTraversable t where
  myTraverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  default myTraverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  myTraverse f = mySequenceA . fmap f

  mySequenceA :: (Applicative f) => t (f a) -> f (t a)
  default mySequenceA :: (Applicative f) => t (f a) -> f (t a)
  mySequenceA = myTraverse id

instance MyTraversable [] where
  myTraverse _ [] = pure []
  myTraverse f (x : xs) = (:) <$> f x <*> traverse f xs

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Functor)

instance Foldable Tree where
  foldMap f (Leaf l) = f l
  foldMap f (Branch lb rb) = foldMap f lb <> foldMap f rb

instance MyTraversable Tree where
  myTraverse f (Leaf l) = fmap Leaf (f l)
  myTraverse f (Branch lb rb) = Branch <$> myTraverse f lb <*> myTraverse f rb

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-- >>> transpose [[1, 2, 3], [4, 5, 6]]
-- [[1,4],[2,5],[3,6]]

myMapAccumLS :: (Traversable t) => (b -> State a c) -> t b -> State a (t c)
myMapAccumLS f xs = traverse f xs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

myMapAccumL :: (Traversable t) => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
myMapAccumL step init xs = swap $ runState (traverse (state . (fmap (fmap swap) (flip step))) xs) init

-- For fmap, all we need is to use Identity to make a traversal out of an arbitrary function:
myFmap :: (Traversable t) => (a -> b) -> t a -> t b
myFmap f = runIdentity . traverse (Identity . f)

-- Const is a constant functor. A value of type Const a b does not actually contain a b value. Rather, it holds an a value which is unaffected by fmap. For our current purposes, the truly interesting instance is the Applicative one whose (<*>) simply combines the values in each context with mappend [6]. We can exploit that to make a traversal out of any Monoid m => a -> m function that we might pass to foldMap. Thanks to the instance above, the traversal then becomes a fold:
myFoldMap :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = getConst . traverse (Const . f)
