module FingerTree where

import Prelude hiding (head, init, last, tail)

data Seq a
  = Nil
  | Unit a
  | More (Some a) (Seq (Tuple a)) (Some a)

data Some a
  = One a
  | Two a a

data Tuple a
  = Pair a a
  | Triple a a a

cons :: a -> Seq a -> Seq a
cons x Nil = Unit x
cons x (More (One y) q u) = More (Two x y) q u
cons x (More (Two y z) q u) = More (One x) (cons (Pair y z) q) u

snoc :: Seq a -> a -> Seq a
snoc Nil x = Unit x
snoc (More u q (One y)) z = More u q (Two y z)
snoc (More u q (Two y z)) x = More u (cons (Pair y z) q) (One x)

head :: Seq a -> a
head Nil = error "head of empty sequence"
head (Unit x) = x
head (More (One x) _ _) = x
head (More (Two x _) _ _) = x

last :: Seq a -> a
last Nil = error "last of empty sequence"
last (Unit x) = x
last (More _ _ (One x)) = x
last (More _ _ (Two _ y)) = y

tail :: Seq a -> Seq a
tail Nil = error "tail of empty sequence"
tail (Unit _) = Nil
tail (More (Two _ x) q u) = More (One x) q u
tail (More (One x) q u) = more0 q u
  where
    more0 Nil (One y) = Unit y
    more0 Nil (Two y z) = More (One y) Nil (One z)
    more0 q u = case head q of
      Pair x y -> More (Two x y) (tail q) u
      Triple x y z -> More (One x) (cons (Pair y z) (map1 chop q)) u

    chop (Triple _ y z) = Pair y z
    chop _ = error "unexpected pattern in chop"

    map1 f Nil = Nil
    map1 f (Unit x) = Unit (f x)
    map1 f (More (One x) q u) = More (One $ f x) q u
    map1 f (More (Two x y) q u) = More (Two (f x) y) q u

init :: Seq a -> Seq a
init Nil = error "init of empty sequence"
init (Unit _) = Nil
init (More u q (Two x _)) = More u q (One x)
init (More u q (One x)) = morel u q
  where
    morel (One y) Nil = Unit y
    morel (Two y z) Nil = More (One y) Nil (One z)
    morel u q = case last q of
      Pair x y -> More u (init q) (Two x y)
      Triple x y z -> More u (mapl chop q) (One z)

    chop (Triple x y _) = Pair x y
    chop _ = error "unexpected pattern in chop"

    mapl f Nil = Nil
    mapl f (Unit x) = Unit (f x)
    mapl f (More u q (One x)) = More u q (One (f x))
    mapl f (More u q (Two x y)) = More u q (Two x (f y))

toList :: Some a -> [a]
toList (One x) = [x]
toList (Two x y) = [x, y]

toTuples :: [a] -> [Tuple a]
toTuples [] = []
toTuples [x, y] = [Pair x y]
toTuples [x, y, z, w] = [Pair x y, Pair z w]
toTuples (x : y : z : xs) = Triple x y z : toTuples xs

-- (++) :: Seq a -> Seq a -> Seq a
-- p ++ q = glue
