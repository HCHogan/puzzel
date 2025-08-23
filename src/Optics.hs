{-# LANGUAGE TemplateHaskell #-}
module Optics where

import Control.Lens
import Control.Lens.Extras
import Data.Char
import Data.List
import Numeric.Lens
import Control.Lens.Unsound

-- >>> (Just 3, Left ("Hello", [1, 2, 3])) & biplate *~100
-- (Just 300,Left ("Hello",[100,200,300]))

-- >>> "why is a raven like a writing desk" & worded . _head %~ toUpper
-- "Why Is A Raven Like A Writing Desk"

-- >>> sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]
-- 27

-- >>> ("one", "two", "three") & partsOf (each . traversed) %~ sort
-- ("eee","hno","orttw")

data Ship = Ship { _name :: String, _numCrew :: Int }
  deriving (Show, Eq)

makeLenses ''Ship

-- >>> :browse Optics
-- unknown command 'browse'

-- to use lensProduct, we should make sure that the two focuses are disjoint
shipInfo :: Lens' Ship (String, Int)
shipInfo = lensProduct name numCrew

-- this is illegal
shipInfo2 :: Lens' Ship (Ship, Int)
shipInfo2 = lensProduct id numCrew

exampleShip :: Ship
exampleShip = Ship "Enterprise" 1000

anotherShip :: Ship
anotherShip = Ship "Voyager" 150

-- >>> view shipInfo2 (set shipInfo2 (anotherShip, 200) exampleShip)
-- (Ship {_name = "Voyager", _numCrew = 200},200)

-- >>> set shipInfo2 (view shipInfo2 exampleShip) exampleShip
-- Ship {_name = "Enterprise", _numCrew = 1000}

-- match :: Query -> Entry -> Maybe Cost
--
-- (Int) -> Int
--
-- async (Bool) -> Int raise Ee
--
-- [K](K) -> Int
--
-- [T](T) -> Int
--
-- (Int) -> Int
-- (Option[Int]) -> Int
--
-- Int -> Int
-- [T : Compare](T) -> Int
--
-- (Int, Int) -> Int
-- Int -> Int -> Int
