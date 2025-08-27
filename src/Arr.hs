{-# LANGUAGE Arrows #-}

module Arr where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b = Circuit {runCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id = Circuit (Cat.id,)
  (Circuit cir2) . (Circuit cir1) = Circuit $ \a ->
    let (cir1', b) = cir1 a
        (cir2', c) = cir2 b
     in (cir2' Cat.. cir1', c)

instance Arrow Circuit where
