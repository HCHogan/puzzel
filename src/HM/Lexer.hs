module HM.Lexer (
  reserved,
  reservedOp,
  identifier,
  integer,
  parens,
  semiSep,
  semi,
  contents,
  ReservedWord (..),
  ReservedOp (..),
  Op,
  Operators,
) where

import Data.Text.Lazy qualified as L
import Text.Parsec
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Text.Lazy
import Text.Parsec.Token qualified as Tok

import Data.Functor.Identity

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

data ReservedWord
  = RWLet
  | RWIn
  | RWFix
  | RWRec
  | RWIf
  | RWThen
  | RWElse
  | RWTrue
  | RWFalse
  deriving (Eq, Ord, Show, Enum, Bounded)

data ReservedOp
  = OpArrow -- "->"
  | OpLambda -- "\\"
  | OpPlus -- "+"
  | OpTimes -- "*"
  | OpMinus -- "-"
  | OpEqEq -- "="
  | OpEq -- "="
  deriving (Eq, Ord, Show, Enum, Bounded)

reservedWordToString :: ReservedWord -> String
reservedWordToString = \case
  RWLet -> "let"
  RWIn -> "in"
  RWFix -> "fix"
  RWRec -> "rec"
  RWIf -> "if"
  RWThen -> "then"
  RWElse -> "else"
  RWTrue -> "True"
  RWFalse -> "False"

reservedOpToString :: ReservedOp -> String
reservedOpToString = \case
  OpArrow -> "->"
  OpLambda -> "\\"
  OpPlus -> "+"
  OpTimes -> "*"
  OpMinus -> "-"
  OpEqEq -> "=="
  OpEq -> "="

reservedWords :: [String]
reservedWords = reservedWordToString <$> [minBound .. maxBound]

reservedOps :: [String]
reservedOps = reservedOpToString <$> [minBound .. maxBound]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = "{-"
      , Tok.commentEnd = "-}"
      , Tok.commentLine = "--"
      , Tok.nestedComments = True
      , Tok.identStart = letter
      , Tok.identLetter = alphaNum <|> oneOf "_'"
      , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.reservedNames = reservedWords
      , Tok.reservedOpNames = reservedOps
      , Tok.caseSensitive = True
      }

reserved :: ReservedWord -> Parser ()
reserved w = Tok.reserved lexer (reservedWordToString w)

reservedOp :: ReservedOp -> Parser ()
reservedOp o = Tok.reservedOp lexer (reservedOpToString o)

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser String
semi = Tok.semi lexer

integer :: Parser Integer
integer = Tok.natural lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof -- we don't need Tok.whiteSpace here because the parser already consumes trailing whitespace
  return r
