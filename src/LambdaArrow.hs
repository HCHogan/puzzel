module LambdaArrow where

import Control.Monad
import Control.Monad.Except

-- On our journey to dependent types, we will start with the simply-typed lambda calculus (λ→).
-- There are four kinds of terms:
-- e ::= e :: τ    -- annotated term
--     | x         -- variable
--     | e e'      -- application
--     | λx -> e   -- abstraction

-- inferable types:
data TermI
  = Ann TermC Type -- An annotated term is inferable
  | Bound Int -- A bounded variable is inferable
  | Free Name -- same for free variable, search in global
  | TermI :@: TermC -- in e (\x -> e') x's type can be determined by the type of e
  deriving (Show, Eq)

-- checkable types:
data TermC
  = Inf TermI -- Inferable types are checkable
  | Lam TermC -- we requrie every lambda-abstracted variable to be explicitly typed
  deriving (Show, Eq)

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

-- There is a set of base types; compound types τ -> τ' correspond to functions from τ to τ'.
-- τ ::= a         -- base types
--     | τ → τ'    -- function type
data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)

-- A value is either a neutral term or a lambda abstraction.
-- v ::= n         -- neutral term
--     | λx -> v   -- lambda abstraction
--
data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

-- A neutral term is something that cannot be used as value but cannot be reduced further:
-- n ::= x         -- variable
--     | n v       -- application
data Neutral
  = NFree Name
  | NApp Neutral Value

-- creates value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

type Env = [Value]

-- The evaluation of λ→ is defined by the following rules.
-- The notaion e ⇓ v means that the result of completely evaluating e is v.
-- To keep the presentation simple, we evaluate everything as far as possible.
evalI :: TermI -> Env -> Value
-- type annotaions are ignored during evaluation:
-- e ⇓ v
-- ———————————————————————
-- e :: τ ⇓ v
evalI (Ann e _) d = evalC e d
-- variables are evaluated to themselves:
-- ———————
--  x ⇓ x
evalI (Free x) d = vfree x
-- the i-th position corresponds to the value of variable i
evalI (Bound i) d = d !! i
evalI (e :@: e') d = vapp (evalI e d) (evalC e' d)
 where
  vapp :: Value -> Value -> Value
  -- When left subterm does yield a lambda abs, we β-reduce:
  -- e ⇓ λx → v   v[x ↦ e′] ⇓ v′
  -- ———————————————————————----
  --      e e′ ⇓ v′
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
evalC (Lam e) d = VLam (\x -> evalC e (x : d))

-- Type ruls are generally of the form Γ ⊢ e :: τ, where Γ is a context and e is a term.
-- The context lists valid base types, and associates identifiers with type information.
-- We wirte a :: * to indicate that a is a base type, x :: t to indicate that x is a
-- term of type t.
-- Every free variable in both terms and types must occur in the context.
-- We write Γ(z) to denote the information associated with identifier z by context Γ.
-- note that λ→ is not polymorphic: a type identifier represents one specific type and
-- cannot be instantiated.
--
-- Context and well-formed types in λ→:
-- Γ ::= ε                    -- empty context
--     | Γ, α :: *            -- adding a type identifier
--     | Γ, x :: τ            -- adding a term identifier
data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

data Kind = Star
  deriving (Show)

-- contexts are implemented as reversed lists associating name with either star or a type
-- extending a context is thus achieved by the list 'cons' operation
-- looking up a name in a context is done by Haskell `lookup` function.
--            valid(Γ)
-- ------------------------------
--        valid(Γ, α :: *)
--
--     valid(Γ)    Γ ⊢ τ :: *
-- ------------------------------
--     valid(Γ, x :: τ)
type Context = [(Name, Info)]

-- the type checking algorithm can fail, we choose a standard error monad to represent:
-- we use Haskell's `throwError` to report an error
type Result a = Either String a

-- check well-formed types:
kind :: Context -> Type -> Kind -> Result ()
-- Γ(α) = *
-- -----------  (TVAR)
-- Γ ⊢ α :: *
kind tau (TFree x) Star = case lookup x tau of
  Just (HasKind Star) -> return ()
  Just (HasType _) -> throwError "Expected a kind, but found a type"
  Nothing -> throwError "Unknown identifier"
--  Γ ⊢ τ :: *      Γ ⊢ τ′ :: *
-- -----------------------------  (FUN)
--   Γ ⊢ τ → τ′ :: *
kind tau (Fun k1 k2) Star = do
  kind tau k1 Star
  kind tau k2 Star

-- the function for inferable terms returns a type, whereas the function for checkable takes
-- a type as input and returns ().

-- Finally, we can give the type rules:
-- ::↓ means the type is supposed to be an input to the type checking algorithm
-- and with ::↑ the type is produced by the type checking algorithm.
typeI :: Int -> Context -> TermI -> Result Type
--        Γ ⊢ τ :: *     Γ ⊢ e ::↓ τ
--     ---------------------------------   (ANN)
--        Γ ⊢ (e :: τ) ::↑ τ
typeI i tau (Ann e t) = do
  kind tau t Star
  typeC i tau e t
  return t
--       Γ(x) = τ
--     -----------------   (VAR)
--       Γ ⊢ x ::↑ τ
typeI i tau (Free x) = do
  case lookup x tau of
    Just (HasType t) -> return t
    Nothing -> throwError "unknown identifier"
--   Γ ⊢ e ::↑ τ → τ′     Γ ⊢ e′ ::↓ τ
--  ------------------------------------   (APP)
--        Γ ⊢ e e′ ::↑ τ′
typeI i tau (e :@: e') = do
  sigma <- typeI i tau e
  case sigma of
    Fun t1 t2 -> do
      typeC i tau e' t1
      return t2
    _ -> throwError "illegal application"

typeC :: Int -> Context -> TermC -> Type -> Result ()
--       Γ ⊢ e ::↑ τ
--     ------------------   (CHK)
--       Γ ⊢ e ::↓ τ
typeC i tau (Inf e) t = do
  t' <- typeI i tau e
  unless (t == t') $ throwError "type mismatch"
--     Γ, x :: τ ⊢ e ::↓ τ′
--  -----------------------------   (LAM)
--   Γ ⊢ λx → e ::↓ τ → τ′
typeC i tau (Lam e) (Fun t t') =
  typeC (i + 1) ((Local i, HasType t) : tau) (substC 0 (Free (Local i)) e) t'
typeC i tau _ _ = throwError "type mismatch"

typeI0 :: Context -> TermI -> Result Type
typeI0 = typeI 0

substI :: Int -> TermI -> TermI -> TermI
substI i r (Ann e t) = Ann (substC i r e) t
substI i r (Bound j) = if i == j then r else Bound j
substI i r (Free y) = Free y
substI i r (e :@: e') = substI i r e :@: substC i r e'

substC :: Int -> TermI -> TermC -> TermC
substC i r (Inf e) = Inf (substI i r e)
substC i r (Lam e) = Lam (substC (i + 1) r e)

-- the use of higher-order abstract syntax requires us to define a quote function that takes a value back to a term.
quote0 :: Value -> TermC
quote0 = quote 0

quote :: Int -> Value -> TermC
quote i (VLam f) = Lam (quote (i + 1) (f (vfree (Quote i))))
quote i (VNeutral n) = Inf (quoteN i n)

quoteN :: Int -> Neutral -> TermI
quoteN i (NFree x) = boundfree i x
quoteN i (NApp n v) = quoteN i n :@: quote i v

boundfree :: Int -> Name -> TermI
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x = Free x

id' = Lam (Inf (Bound 0))

const' = Lam (Lam (Inf (Bound 1)))

tfree a = TFree (Global a)

free x = Inf (Free (Global x))

term1 = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"

term2 = Ann const' (Fun (Fun (tfree "b") (tfree "b")) (Fun (tfree "a") (Fun (tfree "b") (tfree "b"))))

env1 =
  [ (Global "y", HasType (tfree "a"))
  , (Global "a", HasKind Star)
  ]

env2 = (Global "b", HasKind Star) : env1

s f g x = f x (g x)
k x y = x
i x = x
skk = s k k

-- >>> quote0 (evalI term1 [])
-- Inf (Free (Global "y"))

-- >>> quote0 (evalI term2 [])
-- Lam (Lam (Inf (Bound 1)))

-- >>> typeI0 env1 term1
-- Right (TFree (Global "a"))

-- >>> typeI0 env2 term2
-- Right (Fun (Fun (TFree (Global "b")) (TFree (Global "b"))) (Fun (TFree (Global "a")) (Fun (TFree (Global "b")) (TFree (Global "b")))))
