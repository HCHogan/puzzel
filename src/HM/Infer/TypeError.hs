module HM.Infer.TypeError where

import HM.Type

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | UnboundVariable String
  deriving (Eq)
