module HM.TypeError where

import HM.Type

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnboundVariable String
  deriving (Eq)
