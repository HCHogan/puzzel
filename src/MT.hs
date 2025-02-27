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

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

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

testme :: Either String Int
testme = do
  x <- Right 10
  y <- throwError "opps" `catchError` (\err -> pure 1)
  return $ x + y

-- >>> testme
-- Right 11

testmw :: Writer String (Int, String -> String)
testmw = do
  tell "Starting calculation..."
  let x = 1 + 1
  -- (x, w) <- listen
  tell "calculation complelte"
  return (x, reverse)

testmw2 :: Writer String Int
testmw2 = do
  x <- pass testmw
  return (x + 1)

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

type Eval2 a = ExceptT String Identity a

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = throwError ("Variable not found: " ++ n) `maybe` return $ M.lookup n env
eval2a env (Plus e1 e2) = do
  e1' <- eval2a env e1
  e2' <- eval2a env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "Type error"

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i

tick :: (Num s, MonadState s m) => m ()
tick = get >>= put . (+ 1)

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $ runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick
  return $ IntVal i
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  case M.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval5 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval5 (App e1 e2) = do
  tick
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body -> local (const (M.insert n val2 env')) (eval5 body)
    _ -> throwError "Type error in app"
