module Untyped.Parser where

import Data.Char
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)

import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Token qualified as Tok

import Untyped.Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
 where
  ops = ["->", "\\", "+", "*", "-", "="]
  names = []
  style =
    haskellStyle -- a bag of predefined parser to handle chars
      { Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      , Tok.commentLine = "#"
      }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  x <- p
  eof
  return x

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = Var <$> identifier

number :: Parser Expr
number = Lit . LInt . fromInteger <$> natural

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

term :: Parser Expr
term =
  parens expr
    <|> variable
    <|> number
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return $ foldl1 App es

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
