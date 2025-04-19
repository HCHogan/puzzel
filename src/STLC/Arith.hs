module STLC.Arith where

import Control.Monad.Except
import Data.Maybe

-- e ::= True
--       False
--       iszero e
--       succ e
--       pred e
--       if e then e else e
--       0
data Expr
  = Tr
  | Fl
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  | Zero
  deriving (Show, Eq)

nf :: Expr -> Expr
nf t = maybe t nf (eval1 t)

eval :: Expr -> Maybe Expr
eval t = if isVal (nf t) then Just (nf t) else Nothing -- term is stuck

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval1 :: Expr -> Maybe Expr
eval1 (Succ t) = Succ <$> eval1 t
eval1 (Pred Zero) = Just Zero
eval1 (Pred (Succ n)) | isNum n = eval1 n
eval1 (Pred t) = Pred <$> eval1 t
eval1 (IsZero (Succ n)) | isNum n = Just Fl
eval1 (IsZero Zero) = Just Tr
eval1 (IsZero e) = IsZero <$> eval1 e
eval1 (If Tr e2 _) = eval1 e2
eval1 (If Fl _ e3) = eval1 e3
eval1 (If e1 e2 e3) = (\e1' -> If e1' e2 e3) <$> eval1 e1
eval1 _ = Nothing

isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ _) = True
isNum (Pred _) = True
isNum _ = False

data Type
  = TBool
  | TNat
  | TArr Type Type
  deriving (Show, Eq)

type Check a = Except TypeError a

data TypeError = TypeMismatch Type Type

check :: Expr -> Either TypeError Type
check = runExcept . typeof

typeof :: Expr -> Check Type
typeof (Succ a) = do
  ta <- typeof a
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat
typeof (Pred a) = do
  ta <- typeof a
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat
typeof (IsZero a) = do
  ta <- typeof a
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat
typeof (If a b c) = do
  ta <- typeof a
  tb <- typeof b
  tc <- typeof c
  if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
        then throwError $ TypeMismatch ta tb
        else return tc
typeof Tr = return TBool
typeof Fl = return TBool
typeof Zero = return TNat
