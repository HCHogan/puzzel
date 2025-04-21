module RecordUpdates () where

data Rec a = MkRec {foo :: Int, bar :: a, baz :: Double}
  deriving (Show)

f x = x{foo = 5}

-- >>> f example
-- MkRec {foo = 5, bar = False, baz = 3.14}

-- >>> :t (f example)
-- (f example) :: Rec Bool

example = MkRec{foo = 1, bar = False, baz = 3.14}

data Rec2 a = MkRec2 {foo2 :: a, bar2 :: a, baz2 :: Double}
  deriving (Show)

example2 = MkRec2{foo2 = 1, bar2 = 2, baz2 = 3.14}

f2 x = x{foo2 = 5}

g2 x = x{foo2 = 5, bar2 = 6}

-- >>> :t (f2 example2)
-- (f2 example2) :: Rec2 Integer

-- >>> :t (g2 example2)
-- (g2 example2) :: Num a1 => Rec2 a1

type Booly a = Bool
type family Booly2 a where
  Booly2 a = Bool

data Rec3 a = MkRec3 {foo3 :: Booly a, bar3 :: a, baz3 :: Double}
  deriving (Show)

f3 x = x{bar3 = 5}

-- >>> :t (f3 example3)
-- (f3 example3) :: Num a1 => Rec3 a1

example3 = MkRec3{foo3 = True, bar3 = 2, baz3 = 3.14}

data Rec4 a = forall b. (Show b) => MkRec4 {foo4 :: b, bar4 :: a, baz4 :: Double}
deriving instance (Show a) => Show (Rec4 a)

f4 x = x{foo4 = False, bar4 = 5}

-- >>> :t (f4 example4)
-- (f4 example4) :: Num a1 => Rec4 a1

example4 = MkRec4{foo4 = True, bar4 = 2, baz4 = 3.14}
