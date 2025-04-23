module Eval.PHOAS where

data ExprP a
  = VarP a
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Integer

newtype Expr = Expr {unExpr :: forall a. ExprP a}

i :: ExprP a
i = LamP (\a -> VarP a)

k :: ExprP a
k = LamP (\x -> LamP (\y -> VarP x))

s :: ExprP a
s =
  LamP
    ( \f ->
        LamP
          ( \g ->
              LamP
                ( \x ->
                    AppP
                      (AppP (VarP f) (VarP x))
                      (AppP (VarP g) (VarP x))
                )
          )
    )

data Value
  = VLit Integer
  | VFun (Value -> Value)

fromVFun :: Value -> (Value -> Value)
fromVFun (VFun f) = f
fromVFun _ = error "Not a function"

fromVLit :: Value -> Integer
fromVLit (VLit i) = i
fromVLit _ = error "Not a literal"

eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (LamP f) = VFun (ev . f)
  ev (VarP v) = v
  ev (AppP e1 e2) = fromVFun (ev e1) (ev e2)
  ev (LitP n) = VLit n


