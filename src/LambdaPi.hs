module LambdaPi where

-- Polymorphism can be made explicit by interpreting it as a type abstraction. The identity function then takes two arguments: a type a and a value of a. Calls to the new identity function must explicitly instantiate the identity function with a type.

data Vec0 a = Vec0

newtype Vec1 a = Vec1 a

data Vec2 a = Vec2 a a

data Vec3 a = Vec3 a a a

-- We would like to have a single family of types, indexed by the number of elements:
-- forall a :: *, n :: Nat. Vec a n
-- The problem is that type Vec abstracts over the value n.
-- The dependent function space Π generalize the usual function space -> by allowing the range to depend on the domain.
-- The parametric polymorphism known from Haskell can be seen as a special case of dependent fuinction, motivating our use of the symbol ‘∀’.

-- In haskell, one can sometimes 'fake' dependent function space, for instance by defining natural numbers on the type level. Since the type-level numbers are different from the value level natural numbers, one then ends up duplicating a lot of concepts on both levels. Furthermore, even though one can lift certain values to the type level in this fashion, additional effort – in the form of advanced type class programming – is required to perform any computation onsuch types. Using dependent types, we can parameterize our types by values, and as we will shortly see, the normal evaluation rules apply.

--  ∀x:A. B(x)
--  replicate :: ∀n: N. a → Vect n a
--  A === N, B(n) === forall a. a -> Vect n a

-- The evaluation now also extend to types. We must extend the abstract syntax of values accordingly.
-- v, τ ::= n              -- neutral term
--        | ∗              -- the type of types
--        | ∀x :: τ.τ ′    -- dependent function space
--        | λx → v         -- lambda abstraction
-- We now use the symbol τ for values that play the role of types.
