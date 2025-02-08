module MT () where

import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as M
import Data.Maybe

type Name = String
data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp
  deriving (Show)

type Env = M.Map Name Value
data Value = IntVal Integer | FunVal Env Name Exp
  deriving (Show)

eval0 :: Env -> Exp -> Value
eval0 env (Lit n) = IntVal n
eval0 env (Var name) = fromJust (M.lookup name env)
eval0 env (Plus e1 e2) =
  let
    IntVal i1 = eval0 env e1
    IntVal i2 = eval0 env e2
   in
    IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let
    val1 = eval0 env e1
    val2 = eval0 env e2
   in
    case val1 of
      FunVal env' name body -> eval0 (M.insert name val2 env') body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- >>> eval0 M.empty exampleExp
-- IntVal 18

type Eval1 a = Identity a

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit n) = return $ IntVal n
eval1 env (Var name) = return $ fromJust $ M.lookup name env
eval1 env (Plus e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case (val1, val2) of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> error "shit"

-- enva1 env (Lit n)
