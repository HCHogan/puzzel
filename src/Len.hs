{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Len () where

import Control.Lens
import Control.Monad
import Control.Type.Operator
import Data.Functor.Contravariant
import Data.Monoid (Sum (..))

-- Functional references point at parts of values, allowing us to access and modify then functionally.

-- simpest use case for lenses: as a nicer alternative to the vanilla haskell records.
data Point = Point {_positionX :: Double, _positionY :: Double}
  deriving (Show)

data Segment = Segment {_segmentStart :: Point, _segmentEnd :: Point}
  deriving (Show)

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

-- >>> let testSeg = makeSegment (1, 2) (3, 4)
-- >>> _positionY . _segmentEnd $ testSeg
-- 4.0

-- >>> let testSeg = makeSegment (1, 2) (3, 4)
-- >>> let end = _segmentEnd testSeg in testSeg { _segmentEnd = end { _positionY = 2 * _positionY end } }
-- Segment {_segmentStart = Point {_positionX = 1.0, _positionY = 2.0}, _segmentEnd = Point {_positionX = 3.0, _positionY = 8.0}}

-- Lenses allow us to avoid such nastiness:
makeLenses ''Point
makeLenses ''Segment

-- positionY is a reference to a `Double` within a `Point`.
-- >>> :info positionY
-- positionY :: Lens' Point Double
--   	-- Defined at /home/hank/Development/hs/puzzel/src/Len.hs:38:1

-- >>> :info segmentEnd
-- segmentEnd :: Lens' Segment Point
--   	-- Defined at /home/hank/Development/hs/puzzel/src/Len.hs:39:1

-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> view segmentEnd testSeg
-- >>> set segmentEnd (Point 2 4) testSeg
-- Point {_positionX = 2.0, _positionY = 4.0}
-- Segment {_segmentStart = Point {_positionX = 0.0, _positionY = 1.0}, _segmentEnd = Point {_positionX = 2.0, _positionY = 4.0}}

-- lenses are easy to compose:
-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> view (segmentEnd . positionY) testSeg
-- 4.0
_ = view (segmentEnd . positionY) (makeSegment (0, 1) (2, 4))

-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> over (segmentEnd . positionY) (2 *) testSeg
-- Segment {_segmentStart = Point {_positionX = 0.0, _positionY = 1.0}, _segmentEnd = Point {_positionX = 2.0, _positionY = 8.0}}

-- Manipulating values within a Traversable structure, as traverse allows us to, is an example of targeting parts of a whole. As flexible as it is, however, traverse only handles a rather limited range of targets. For one, we might want to walk across structures that are not Traversable functors. Here is an entirely reasonable function that does so with our Point type:
pointCorrdiantes :: (Applicative f) => (Double -> f Double) -> Point -> f Point
pointCorrdiantes g (Point x y) = Point <$> g x <*> g y

-- This generalised notion of a traversal that pointCoordinates exemplifies is captured by one of the core types of lens: Traversal.
type MyTraversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t

extremityCorrdiantes :: (Applicative f) => (Double -> f Double) -> Segment -> f Segment
extremityCorrdiantes g (Segment start end) = Segment <$> pointCorrdiantes g start <*> pointCorrdiantes g end

-- Next our programme comes the generalisation of the links between `Traversable`, `Functor` and `Foldable`.
-- We recover fmap from traverse by picking Identity as the applicative functor, in lens parlance, that is how you get a `Setter`.
type Setter s t a b = forall f. (Settable f) => (a -> f b) -> s -> f t

-- over is the essential combinator for setters, it works a lot like fmap, except that you pass a setter as its first argument in order to specify which parts of the structure you want to target
-- >>> over pointCorrdiantes negate (makePoint (1, 2))
-- Point {_positionX = -1.0, _positionY = -2.0}

-- >>> over mapped negate [1..4]
-- >>> over mapped negate (Just 3)
-- [-1,-2,-3,-4]
-- Just (-3)

-- another very important combinator is set, which replaces all targeted values with a constant. set setter x = over setter (const x), analogously to how (x <$) = fmap (const x)
-- >>> set pointCorrdiantes 7 (makePoint (1, 2))
-- Point {_positionX = 7.0, _positionY = 7.0}

scaleSegment :: Double -> Segment -> Segment
scaleSegment = set extremityCorrdiantes

-- myMapped :: Functor f => MyTraversal (f a) (f b) a b
myMapped g a = Identity $ fmap (runIdentity . g) a

-- instance Contravariant (`(->)` r) where
--   contramap f g = g . f

-- >>> let largerThanFour = Predicate (> 4)
-- >>> getPredicate (contramap length largerThanFour) "hello"
-- True

-- its time to do the same with foldMap-as-traversal.
-- forall r. Monoid r => (a -> Const r a) -> s -> Const r s
-- Control.Lens.Fold uses something slightly more general than `Monoid r => Const r`:
type MyFold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- `Monoid r => Const r` is both Contravariant and Applicative. Thanks to the functor and contravariant laws, anything that is both a Contravariant and a Functor is, just like Const r, a vacuous functor, with both fmap and contramap doing nothing. The additional Applicative constraint corresponds to the Monoid r; it allows us to actually perform the fold by combining the Const-like contexts created from the targets.

-- Every Traversal can be used as a Fold, given that a `Traversal` must work with any Applicative, including those are also Contravariant. The situation parallels exactly what we have seen for `Traversal` and `Setter`.
-- Control.Lens.Fold offers analogues to everything in Data.Foldable. Two commonly seen combinators from that module are toListOf, which produces a list of the Fold targets...
-- and preview, which extracts the first target of a Fold using the First monoid from Data.Monoid.
-- >>> toListOf extremityCorrdiantes (makeSegment (1, 2) (3, 4))
-- >>> preview traverse [1..10]
-- [1.0,2.0,3.0,4.0]
-- Just 1

-- relax the `Applicative` constraint to merely `Functor`, we obtain a `Getter`.
type MyGetter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- As f still has to be both Contravariant and Functor, it remains being a Const-like vacuous functor. Without the Applicative constraint, however, we can't combine results from multiple targets. The upshot is that a Getter always has exactly one target, unlike a Fold (or, for that matter, a Setter, or a Traversal) which can have any number of targets, including zero.
-- someGetter :: (a -> r a) -> s -> Const r s
-- A Getter s a is equivalent to a s -> a function. From this point of view, it is only natural that it takes exactly one target to exactly one result. It is not surprising either that two basic combinators from Control.Lens.Getter are to, which makes a Getter out of an arbitrary function, and view, which converts a Getter back to an arbitrary function.
-- same as fst (4, 1)
-- >>> view (to fst) (4, 1)
-- 4

-- >>> view traverse (fmap Sum [1..10])
-- >>> view both ([1,2], [3,4,5])
-- Sum {getSum = 55}
-- [1,2,3,4,5]

-- Semantic editor combinators:
-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- >>> :t (.).(.)
-- (.).(.) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c

-- >>> :t traverse . traverse . traverse
-- traverse . traverse . traverse
--   :: (Applicative f, Traversable t1, Traversable t2,
--       Traversable t3) =>
--      (a -> f b) -> t1 (t2 (t3 a)) -> f (t1 (t2 (t3 b)))

-- type SEC s t a b = (a -> b) -> s -> t
-- functions like fmap, first can be extend to this type

-- you can get fmap by providing the identity functor:
-- fmapDefault f = runIdentity . traverse (Identity . f)
-- if we pass the traverse as an argument:
-- over l f = runIdentity . l (Identity . f)
-- over :: ((a -> Identity b) -> s -> Identtity t) -> (a -> b) -> s -> t
-- over traverse f === runIdentity . traverse (Identity f) === fmapDefault f === fmap f
-- the type system will automatically choose Identity as the functor userd inside traverse here
-- type Setter s t a b = (a -> Identity b) -> s -> Identity t
-- mapped :: Functor f => Setter (f a) (f b) a b
-- mapped f = Identity . fmap (runIdentity . fmap)
-- over mapped f = runIdentity . mapped (Identity . f) === runIdentity . Identity . fmap (runIdentity . Identity f)
-- === fmap f

-- we can make a setter for Data.Text:
-- chars :: Setter Text Text Char Char === (Char -> Identity Char) -> Text -> Identity Text
-- chars f = fmap pack . mapped f . unpack
-- we can even make setters that are not valid functor
-- both :: Setter (a , a) (b, b) a b
-- both f (a, b) = (,) <$> f a <*> f b
-- first :: Setter (a, c) (b, c) a b
-- first f (a, b) = (,b) <$> f a

-- >>> (1, 2) & _1 .~ "hello" & _2 .~ "World"
-- ("hello","World")

-- >>> (("Alice", 1), ("Bob", 2)) ^.. folded . _1
-- ["Bob"]

-- >>> import Data.Map as Map
-- >>> Map.fromList [("hello", 4)] & at "hello" .~ Nothing
-- fromList []

-- >>> (1, 2) ^.. both
-- [1,2]

-- >>> [(1,2),(3,4),(5,6)] ^.. traverse . both
-- [1,2,3,4,5,6]

-- >>> import Data.Aeson.Lens
-- >>> import Data.Aeson
-- >>> let json = "[{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]"
-- >>> names = json ^.. values . key "name" . _String
-- ["Alice","Bob"]

-- In nutshell:
-- type Traversal s t a b = forall f. Applicative f => (a -> f b)        -> s -> f t
-- type Lens      s t a b = forall f. Functor f     => (a -> f b)        -> s -> f t
-- type Setter    s t a b =                            (a -> Identity b) -> s -> Identity t
-- type Fold      s   a   = forall m. Monoid m      => (a -> Const m a)  -> s -> Const m s
-- type Geting  r s t a b =                            (a -> Const r b)  -> s -> Const r t
--
-- Some useful alias:
-- type Getter     s a    = Getting a s s a a
-- type SimpleLens s a    = type Lens' s a = Lens s s a a
