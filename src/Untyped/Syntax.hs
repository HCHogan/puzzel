module Untyped.Syntax where

type Name = String

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Expr
  deriving (Show, Eq)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq)
