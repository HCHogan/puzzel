module Playground (RegExp (..), parseRegExp) where

import Text.Parsec
import Text.Parsec.String (Parser)

class Collects s where
  insert :: s a -> a -> s a

instance Collects [] where
  insert xs x = x : xs

data RegExp
  = -- | A character that is not in "()*|."
    Normal Char
  | -- | Any character
    Any
  | -- | Zero or more occurances of the same regexp
    ZeroOrMore RegExp
  | -- | A choice between 2 regexps
    Or RegExp RegExp
  | -- | A sequence of regexps.
    Str [RegExp]
  deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse (regParser <* eof) "" s of
  Left _ -> Nothing
  Right r -> Just r

normalParser :: Parser RegExp
normalParser = Normal <$> noneOf "()*|."

anyCharParser :: Parser RegExp
anyCharParser = Any <$ char '.'

atomParser :: Parser RegExp
atomParser = normalParser <|> anyCharParser <|> parenParser

zeroOrMoreParser :: Parser RegExp
zeroOrMoreParser = do
  e <- atomParser
  option e $ ZeroOrMore e <$ char '*'

sequenceParser :: Parser RegExp
sequenceParser = do
  terms <- many1 zeroOrMoreParser
  case terms of
    [x] -> return x
    xs -> return $ Str xs

orParser :: Parser RegExp
orParser = do
  l <- sequenceParser
  char '|'
  r <- sequenceParser
  notFollowedBy $ char '|'
  return $ Or l r

parenParser :: Parser RegExp
parenParser = do
  char '('
  r <- regParser
  char ')'
  return r

-- regParser =

regParser :: Parser RegExp
regParser = try orParser <|> sequenceParser
