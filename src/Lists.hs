{-# LANGUAGE GADTs #-}

module Lists () where

import Control.Monad
import Data.Char (toLower)
import Data.Dynamic
import Data.Kind
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Type.Equality
import Type.Reflection

data Sigma :: (Type -> Type) -> Type where
  MkSigma :: p a -> a -> Sigma p

castSigma :: TypeRep a -> Sigma TypeRep -> Maybe a
castSigma tr (MkSigma tr' a) = case testEquality tr tr' of
  Just Refl -> Just a
  Nothing -> Nothing

castSigma' :: (Typeable a) => Sigma TypeRep -> Maybe a
castSigma' = castSigma typeRep

findValueOfType :: (Typeable a) => HList TypeRep -> Maybe a
findValueOfType = listToMaybe . mapMaybe castSigma'

showIfBool :: Sigma TypeRep -> String
showIfBool (MkSigma p x) = case testEquality p (typeRep @Bool) of
  Just Refl -> show x
  Nothing -> "Not a Bool"

-- >>> let x = MkSigma TypeRep True
-- >>> let y = MkSigma TypeRep (4 :: Int)
-- >>> (showIfBool x, showIfBool y)
-- ("True","Not a Bool")
-- ("True","Not a Bool")

showIfBoolDynamic :: Dynamic -> String
showIfBoolDynamic dyn = case fromDynamic dyn of
  Just x -> if x then "True" else "False"
  Nothing -> "Not a Bool"

data Showable :: Type -> Type where
  WithShowable :: (Show a) => Showable a

justAnInt :: Sigma ((:~:) Int)
justAnInt = MkSigma Refl 10

type HList p = [Sigma p]

data Method = HTTP | HTTPS

indexHList :: Int -> HList p -> Maybe (Sigma p)
indexHList _ [] = Nothing
indexHList 0 (x : _) = Just x
indexHList n (_ : xs) = indexHList (n - 1) xs

-- this hlist can be used as parameters in dynamic typed languages
mkConnection :: HList TypeRep -> IO ()
mkConnection hlist = doTheThing host port method
 where
  host :: Maybe String
  host = findValueOfType hlist
  port :: Maybe Int
  port = findValueOfType hlist
  method :: Maybe Method
  method = findValueOfType hlist
  doTheThing :: Maybe String -> Maybe Int -> Maybe Method -> IO ()
  doTheThing = error "todo"

showAll :: HList Showable -> [String]
showAll = map (\(MkSigma WithShowable x) -> show x)

-- >>> showAll [MkSigma WithShowable 10, MkSigma WithShowable True]
-- ["10","True"]

data SomeList :: (Type -> Type) -> Type where
  MkSomeList :: p a -> [a] -> SomeList p

data Comparable :: Type -> Type where
  WithOrd :: (Ord a) => Comparable a

monotonic :: (Ord a) => [a] -> Bool
monotonic [] = True
monotonic [_] = True
monotonic (x : y : xs) = x <= y && monotonic (y : xs)

monotonicSomeList :: SomeList Comparable -> Bool
monotonicSomeList (MkSomeList WithOrd xs) = monotonic xs

getItems :: IO (SomeList Comparable)
getItems = do
  print "int or bool or string?"
  ans <- getLine
  case toLower <$> ans of
    "int" -> MkSomeList WithOrd <$> replicateM 3 (readLn @Int)
    "bool" -> MkSomeList WithOrd <$> replicateM 3 (readLn @Bool)
    "string" -> MkSomeList WithOrd <$> replicateM 3 getLine
    _ -> error "invalid input"

getAndAnalyze :: IO ()
getAndAnalyze = do
  MkSomeList WithOrd xs <- getItems
  print $ "Got" ++ show (length xs) ++ "items"
  let isMono = monotonic xs
  when isMono $ print "Monotonic!"
  unless isMono $ print "Not Monotonic!"

processList :: SomeList TypeRep -> Bool
processList (MkSomeList tr xs)
  | Just Refl <- testEquality tr (typeRep @Int) = True
  | Just Refl <- testEquality tr (typeRep @Bool) = True
  | Just Refl <- testEquality tr (typeRep @String) = True
  | otherwise = False

-- In this specific situation, using a closed ADT of all the types you’d actually want is probably preferred (like data Value = VBool Bool | VInt Int | VDouble Double | VString String), since we only ever get one of four different types. Using Comparable like this gives you a completely open type that can take any instance of Ord, and using TypeRep gives you a completely open type that can take literally anything.
