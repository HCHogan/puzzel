{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Len () where

import Control.Lens

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
--   	-- Defined at /home/hank/Development/hs/puzzel/src/Len.hs:32:1

-- >>> :info segmentEnd
-- segmentEnd :: Lens' Segment Point
--   	-- Defined at /home/hank/Development/hs/puzzel/src/Len.hs:33:1

-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> view segmentEnd testSeg
-- >>> set segmentEnd (Point 2 4) testSeg
-- Point {_positionX = 2.0, _positionY = 4.0}
-- Segment {_segmentStart = Point {_positionX = 0.0, _positionY = 1.0}, _segmentEnd = Point {_positionX = 2.0, _positionY = 4.0}}

-- lenses are easy to compose:
-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> view (segmentEnd . positionY) testSeg
-- 4.0

-- >>> let testSeg = makeSegment (0, 1) (2, 4)
-- >>> over (segmentEnd . positionY) (2 *) testSeg
-- Segment {_segmentStart = Point {_positionX = 0.0, _positionY = 1.0}, _segmentEnd = Point {_positionX = 2.0, _positionY = 8.0}}

pointCorrdiantes :: (Applicative f) => (Double -> f Double) -> Point -> f Point
pointCorrdiantes g (Point x y) = Point <$> g x <*> g y

-- This generalised notion of a traversal that pointCoordinates exemplifies is captured by one of the core types of lens: Traversal.
type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t

extremityCorrdiantes :: (Applicative f) => (Double -> f Double) -> Segment -> f Segment
extremityCorrdiantes g (Segment start end) = Segment <$> pointCorrdiantes g start <*> pointCorrdiantes g end


