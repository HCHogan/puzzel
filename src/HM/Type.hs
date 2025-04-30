module HM.Type where

import HM.Syntax
import Data.Map qualified as M

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq)

infixr 9 `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

