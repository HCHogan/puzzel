module Eval.IO where

import Control.Monad
import Data.Functor

data ExprP a
  = VarP a
  | GlobalP Name
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Char
  | EffectP a

newtype Expr = Expr {unExpr :: forall a. ExprP a}

type Name = String

data Value
  = VChar Char
  | VFun (Value -> Value)
  | VEffect (IO Value)
  | VUnit

fromVEff :: Value -> IO Value
fromVEff (VEffect eff) = eff
fromVEff _ = error "not an effect"

fromVFun :: Value -> (Value -> Value)
fromVFun (VFun f) = f
fromVFun _ = error "not an function"

fromVChar :: Value -> Char
fromVChar (VChar c) = c
fromVChar _ = error "not a char"

eval :: Expr -> Value
eval e = ev (unExpr e)
 where
  ev :: ExprP Value -> Value
  ev (VarP v) = v
  ev (AppP e1 e2) = fromVFun (ev e1) (ev e2)
  ev (LamP f) = VFun (ev . f)
  ev (LitP c) = VChar c
  ev (EffectP e) = e
  ev (GlobalP op) = prim op

-- simply do a lookup on builtin functions
prim :: Name -> Value
prim "putChar#" = unary $ \x ->
  VEffect $ VUnit <$ putChar (fromVChar x)
prim "getChar#" = VEffect $ VChar <$> getChar
prim "bindIO#" = binary $ \x y -> bindIO x y
prim "returnIO#" = unary $ \x -> returnIO x
prim "thenIO#" = binary $ \x y -> thenIO x y

bindIO :: Value -> Value -> Value
bindIO (VEffect f) (VFun g) = VEffect (f >>= fromVEff . g)
bindIO _ _ = error "not a bind"

returnIO :: Value -> Value
returnIO a = VEffect $ return a

thenIO :: Value -> Value -> Value
thenIO (VEffect f) (VEffect g) = VEffect $ f *> g

run :: Expr -> IO ()
run = void . fromVEff . eval

unary :: (Value -> Value) -> Value
unary = VFun

binary :: (Value -> Value -> Value) -> Value
binary f = VFun $ \a -> VFun $ \b -> f a b


