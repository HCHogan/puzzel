module HM.Parser (
  parseExpr,
  parseModule,
) where

import Text.Parsec
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token qualified as Tok

import Data.Functor
import Data.Text.Lazy qualified as L

import HM.Lexer
import HM.Syntax

{-
  source
    ↓ whiteSpace + contents
  [ aexp parser ]  -- parens / literals / keywords / if
    ↓ many1
  [ term parser ]  -- function application
    ↓ buildExpressionParser
  [ expr parser ]  -- infix ops
    ↓ many top‐level 分支
  [ decl / modl ]  -- module (TODO)
    ↓ eof
  AST
-}

variable :: Parser Expr
variable = Var <$> identifier

number :: Parser Expr
number = Lit . LInt . fromInteger <$> integer

bool :: Parser Expr
bool =
  (reserved RWTrue $> Lit (LBool True))
    <|> (reserved RWFalse $> Lit (LBool False))

lambda :: Parser Expr
lambda = do
  reservedOp OpLambda
  args <- many identifier
  reservedOp OpArrow
  body <- expr
  return $ foldr Lam body args

fix :: Parser Expr
fix = do
  reserved RWFix
  Fix <$> expr

letin :: Parser Expr
letin = do
  reserved RWLet
  x <- identifier
  reservedOp OpEq
  e1 <- expr
  reserved RWIn
  Let x e1 <$> expr

-- Bogus: use let + fix
letrecin :: Parser Expr
letrecin = do
  reserved RWLet
  reserved RWRec
  x <- identifier
  reservedOp OpEq
  e1 <- expr
  reserved RWIn
  Let x e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved RWIf
  cond <- expr
  reserved RWThen
  tr <- expr
  reserved RWElse
  If cond tr <$> expr

aexp :: Parser Expr
aexp = choice [parens expr, bool, number, ifthen, fix, try letrecin, letin, lambda, variable]

term :: Parser Expr
term = do
  x <- aexp
  xs <- many aexp
  return $ foldl App x xs

infixOp :: ReservedOp -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp op f = Ex.Infix (reservedOp op $> f)

opTable :: Operators Expr
opTable =
  [ [infixOp OpTimes (Op Mul) Ex.AssocLeft]
  ,
    [ infixOp OpPlus (Op Add) Ex.AssocLeft
    , infixOp OpMinus (Op Sub) Ex.AssocLeft
    ]
  , [infixOp OpEqEq (Op Eql) Ex.AssocLeft]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable term

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved RWLet
  name <- identifier
  args <- many identifier
  reservedOp OpEq
  body <- expr
  return (name, foldr Lam body args)

letrecdecl :: Parser Binding
letrecdecl = do
  reserved RWLet
  reserved RWRec
  name <- identifier
  args <- many identifier
  reservedOp OpEq
  body <- expr
  return (name, Fix $ foldr Lam body (name : args))

val :: Parser Binding
val = (,) "it" <$> expr

decl :: Parser Binding
decl = try letrecdecl <|> try letdecl <|> val

top :: Parser Binding
top = decl <* optional semi

modl :: Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule :: FilePath -> L.Text -> Either ParseError [Binding]
parseModule = parse (contents modl)
