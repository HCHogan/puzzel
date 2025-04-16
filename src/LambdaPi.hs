module LambdaPi where

import Control.Monad
import Control.Monad.Except

-- Polymorphism can be made explicit by interpreting it as a type abstraction. The identity function then takes two arguments: a type a and a value of a. Calls to the new identity function must explicitly instantiate the identity function with a type.

data Vec0 a = Vec0

newtype Vec1 a = Vec1 a

data Vec2 a = Vec2 a a

data Vec3 a = Vec3 a a a

-- We would like to have a single family of types, indexed by the number of elements:
-- forall a :: *, n :: Nat. Vec a n
-- The problem is that type Vec abstracts over the value n.
-- The dependent function space Π generalize the usual function space -> by allowing the range to depend on the domain.
-- ThQuantityarametric polymorphism known from Haskell can be seen as a special case of dependent fuinction, motivating our use of the symbol ‘∀’.

-- In haskell, one can sometimes 'fake' dependent function space, for instance by defining natural numbers on the type level. Since the type-level numbers are different from the value level natural numbers, one then ends up duplicating a lot of concepts on both levels. Furthermore, even though one can lift certain values to the type level in this fashion, additional effort – in the form of advanced type class programming – is required to perform any computation onsuch types. Using dependent types, we can parameterize our types by values, and as we will shortly see, the normal evaluation rules apply.

--  ∀x:A. B(x)
--  replicate :: ∀n: N. a → Vect n a
--  A === N, B(n) === forall a. a -> Vect n a
--
--  We no longer have the need for a separate syntactic category of types or kinds, all constructs for all levels are now integrated into the term langauge.
--  e, p, k ::= e :: p      -- annotated term
--            | *           -- the type of types
--            | ∀x :: p. p' -- dependent function space
--            | x           -- variable
--            | e e'        -- application
--            | λx → e      -- lambda abstraction
data TermI
  = Ann TermC TermC
  | Star
  | Pi TermC TermC
  | Bound Int
  | Free Name
  | TermI :@: TermC
  deriving (Show, Eq)

data TermC
  = Inf TermI
  | Lam TermC
  deriving (Show, Eq)

-- The evaluation now also extend to types. We must extend the abstract syntax of values accordingly.
-- v, τ ::= n               -- neutral term
--        | ∗               -- the type of types
--        | ∀x :: τ.τ ′     -- dependent function space
--        | λx → v          -- lambda abstraction
-- We now use the symbol τ for values that play the role of types.
data Value
  = VLam (Value -> Value)
  | VStar
  -- x is visible in B (Value -> Value), not visible in A (Value)
  | VPi Value (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

type Type = Value
type Env = [Value]

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

evalI :: TermI -> Env -> Value
-- type annotaions are ignored during evaluation:
-- e ⇓ v
-- ———————————————————————
-- e :: τ ⇓ v
evalI (Ann e _) d = evalC e d
-- variables are evaluated to themselves:
-- ———————
--  x ⇓ x
evalI (Free n) d = vfree n
-- eval of star is trivial
evalI Star d = VStar
-- the i-th position corresponds to the value of variable i
evalI (Bound i) d = d !! i
-- ρ ⇓ τ      ρ' ⇓ τ'
-- --------------------------
-- ∀x :: ρ.ρ' ⇓ ∀x :: τ.τ'
evalI (Pi t t') d = VPi (evalC t d) (\x -> evalC t' (x : d))
evalI (e :@: e') d = vapp (evalI e d) (evalC e' d)
  where
    -- When left subterm does yield a lambda abs, we β-reduce:
    -- e ⇓ λx → v   v[x ↦ e′] ⇓ v′
    -- ———————————————————————----
    --      e e′ ⇓ v′
    vapp :: Value -> Value -> Value
    vapp (VLam f) v = f v
    -- When left subterm yield a neutral term, the evaluation cannot proceed:
    -- e ⇓ n   e′ ⇓ v′
    -- ———————————————————
    --   e e′ ⇓ n v′
    vapp (VNeutral n) v = VNeutral (NApp n v)

evalC :: TermC -> Env -> Value
evalC (Inf i) d = evalI i d
-- e ⇓ v
-- ———————————————————
-- λx → e ⇓ λx → v
evalC (Lam t) d = VLam (\x -> evalC t (x : d))

type Result a = Either String a
-- The context of λΠ is defined by the following rules.
-- The precondition Γ ⊢ τ ::↓ * no longer refers to a apecial judgement for the well-formed type
-- but to ensure that τ does not contain unknown variables.
-- Γ ::= ε                  -- empty context
--     | Γ, x :: τ          -- adding a variable
--
--
-- ------------------------------
--        valid(ε)
--
--
--    valid(Γ)    Γ ⊢ τ ::↓ *
-- ------------------------------
--        valid(Γ, x :: τ)

-- the distinction between inferable and checkable terms ensures that the only place where we need to apply conversion rule is (ANN).
type Context = [(Name, Type)]


-- (CHK)
-- Γ ⊢ e ::↑ τ
-- ----------------
-- Γ ⊢ e ::↓ τ

-- (LAM)
-- Γ, x :: τ ⊢ e ::↓ τ'
-- ---------------------------
-- Γ ⊢ λx → e ::↓ ∀x :: τ.τ'

typeI :: Int -> Context -> TermI -> Result Type
-- (ANN)
-- Γ ⊢ ρ ::↓ *  ρ ⇓ τ  Γ ⊢ e ::↓ τ
-- -----------------------
-- Γ ⊢ (e :: ρ) ::↑ τ
typeI i tau (Ann e p) = do
  typeC i tau p VStar
  let t = evalC p []
  typeC i tau e t
  return t
-- (STAR)
-- ----------
-- Γ ⊢ * ::↑ *
typeI i tau Star = return VStar
-- Γ ⊢ ρ ::↓ *     ρ ⇓ τ
-- Γ, x :: τ ⊢ ρ' ::↓ *
-- ----------------------- (PI)
-- Γ ⊢ ∀x :: ρ.ρ' ::↑ *
typeI i tau (Pi p p') = do
  let t = evalC p []
  typeC (i + 1) ((Local i, t):tau) (substC 0 (Free (Local i))p') VStar
  return VStar
-- (VAR)
-- Γ(x) = τ
-- -------------
-- Γ ⊢ x ::↑ τ
typeI i tau (Free x) = do
  case lookup x tau of
    Just t -> return t
    Nothing -> throwError "unknown identifier"
-- Γ ⊢ e ::↑ ∀x :: τ.τ'    Γ ⊢ e' ::↓ τ     τ'[x ↦ e'] ⇓ τ''
-- -------------------------------------------------------------- (APP)
--                Γ ⊢ e e' ::↑ τ''
typeI i tau (e :@: e') = do
  sigma <- typeI i tau e
  case sigma of
    VPi t t' -> do
      typeC i tau e' t
      return (t' (evalC e' []))
    _ -> throwError "illegal application"

typeC :: Int -> Context -> TermC -> Type -> Result ()
typeC = undefined
-- typeC i tau (Inf e) v = do
--   v' <- typeI i tau e
--   unless (quote)

substC :: Int -> TermI -> TermC -> TermC
substC = undefined

substI :: Int -> TermI -> TermI -> TermI
substI = undefined
