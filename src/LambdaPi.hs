module LambdaPi where

-- On our journey to dependent types, we will start with the simply-typed lambda calculus (λ→).
-- There is a set of base types; compound types τ -> τ' correspond to functions from τ to τ'.
-- τ ::= a         -- base types
--     | τ → τ'    -- function type
--
-- There are four kinds of terms:
-- e ::= e :: τ    -- annotated term
--     | x         -- variable
--     | e e'      -- application
--     | λx -> e   -- abstraction
--
-- A value is either a neutral term or a lambda abstraction.
-- v ::= n         -- neutral term
--     | λx -> v   -- lambda abstraction
--
-- n ::= x         -- variable
--     | n v       -- application
--
-- The evaluation of λ→ is defined by the following rules.
-- The notaion e ⇓ v means that the result of completely evaluating e is v.
-- To keep the presentation simple, we evaluate everything as far as possible.
--
-- type annotaions are ignored during evaluation:
-- e ⇓ v
-- ———————————————————————
-- e :: τ ⇓ v
--
-- variables are evaluated to themselves:
-- ———————
--  x ⇓ x
--
-- When left subterm does yield a lambda abs, we β-reduce:
-- e ⇓ λx → v   v[x ↦ e′] ⇓ v′
-- ———————————————————————----
--      e e′ ⇓ v′
--
-- When left subterm yield a neutral term, the evaluation cannot proceed:
-- e ⇓ n   e′ ⇓ v′            
-- ———————————————————        
--   e e′ ⇓ n v′              
--
-- e ⇓ v
-- ———————————————————
-- λx → e ⇓ λx → v
--
--
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

--            valid(Γ)
-- ------------------------------
--        valid(Γ, α :: *)

--     valid(Γ)    Γ ⊢ τ :: *
-- ------------------------------
--     valid(Γ, x :: τ)

-- Γ(α) = *
-- -----------  (TVAR)
-- Γ ⊢ α :: *

--  Γ ⊢ τ :: *      Γ ⊢ τ′ :: *
-- -----------------------------  (FUN)
--   Γ ⊢ τ → τ′ :: *
--
--
-- Finally, we can give the type rules:
-- ::↓ means the type is supposed to be an input to the type checking algorithm
-- and with ::↑ the type is produced by the type checking algorithm.
--        Γ ⊢ τ :: *     Γ ⊢ e ::↓ τ
--     ---------------------------------   (ANN)
--        Γ ⊢ (e :: τ) ::↑ τ

--       Γ(x) = τ
--     -----------------   (VAR)
--       Γ ⊢ x ::↑ τ

--   Γ ⊢ e ::↑ τ → τ′     Γ ⊢ e′ ::↓ τ
--  ------------------------------------   (APP)
--        Γ ⊢ e e′ ::↑ τ′

--       Γ ⊢ e ::↑ τ
--     ------------------   (CHK)
--       Γ ⊢ e ::↓ τ

--     Γ, x :: τ ⊢ e ::↓ τ′
--  -----------------------------   (LAM)
--   Γ ⊢ λx → e ::↓ τ → τ′
--

-- inferable types:
data STTermI
  = Ann STTermC Type
  | Bound Int
  | Free Name
  | STTermI :@: STTermC
  deriving (Show, Eq)

-- checkable types:
data STTermC
  = Inf STTermI
  | Lam STTermC
  deriving (Show, Eq)

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)
