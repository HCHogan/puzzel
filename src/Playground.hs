{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Playground (RegExp (..), parseRegExp) where

import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.STRef
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.String (Parser)

newtype Stack a = Stack {unstack :: StateT Int (WriterT [Int] IO) a}

foo :: Stack ()
foo = Stack $ do
  put 1
  tell [2]
  modify (+ 1)
  liftIO $ print 3
  return ()

data MyContext = MyContext {ffoo :: String, bar :: Int}

str :: T.Text
str = "Far"

computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks ffoo
  if n > 0 then return (Just x) else return Nothing

-- >>> runReader computation $ MyContext "hello" 1
-- Just "hello"

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unstack m) 0)

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

counterExample :: Int
counterExample = runST $ do
  ref <- newSTRef 0
  modifySTRef ref (+ 1)
  readSTRef ref

-- _ = runST (newSTRef 1)

class C a where
  method :: a -> String

class (C a) => D a

class (C a) => E a

-- instance (E b, b ~ Maybe a) => D (Maybe a)
