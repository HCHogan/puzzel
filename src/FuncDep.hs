{-# LANGUAGE FunctionalDependencies #-}

module FuncDep () where

data Vector = Vector Int Int deriving (Eq, Show)

data Matrix = Matrix Vector Vector deriving (Eq, Show)

-- instance Num Vector where
--   Vector x1 y1 + Vector x2 y2 = Vector (x1 + x2) (y1 + y2)
--   Vector x1 y1 - Vector x2 y2 = Vector (x1 - x2) (y1 - y2)
--   Vector x1 y1 * Vector x2 y2 = Vector (x1 * x2) (y1 * y2)
--   abs (Vector x y) = Vector (abs x) (abs y)
--   signum (Vector x y) = Vector (signum x) (signum y)
--   fromInteger x = Vector (fromInteger x) (fromInteger x)
--
-- instance Num Matrix where
--   Matrix v1 v2 + Matrix v3 v4 = Matrix (v1 + v3) (v2 + v4)
--   Matrix v1 v2 - Matrix v3 v4 = Matrix (v1 - v3) (v2 - v4)
--   Matrix v1 v2 * Matrix v3 v4 = Matrix (v1 * v3) (v2 * v4)
--   abs (Matrix v1 v2) = Matrix (abs v1) (abs v2)
--   signum (Matrix v1 v2) = Matrix (signum v1) (signum v2)
--   fromInteger x = Matrix (fromInteger x) (fromInteger x)

class Mult a b c | a b -> c where
  (*) :: a -> b -> c


-- instance Mult Vector Matrix Vector where
--   Vector x1 y1 * Matrix (Vector x2 y2) (Vector x3 y3) = Vector (x1 * x2 + y1 * x3) (x1 * y2 + y1 * y3)
--
-- instance Mult Matrix Vector Vector where
--   Matrix (Vector x1 y1) (Vector x2 y2) * Vector x3 y3 = Vector (x1 * x3 + x2 * y3) (y1 * x3 + y2 * y3)
--
-- instance Mult Matrix Matrix Matrix where

