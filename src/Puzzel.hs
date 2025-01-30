{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Puzzel (answers) where

import Control.Monad
import Data.List
import Data.Maybe

data Orangutan = Merah | Ofallo | Quirrel | Shamir
  deriving (Enum, Show, Bounded, Eq)

data Handler = Dolly | Eva | Francine | Gracie
  deriving (Enum, Show, Bounded, Eq)

data Location = Ambalat | Basahan | Kendisi | Tarakan
  deriving (Enum, Show, Bounded, Eq)

orangutans :: [Orangutan]
orangutans = [minBound .. maxBound]

handlers :: [Handler]
handlers = [minBound .. maxBound]

locations :: [Location]
locations = [Ambalat, Basahan, Kendisi, Tarakan]

ages :: [Int]
ages = [4, 7, 10, 13]

data Assignment
  = MkAssignment {orangutan :: Orangutan, handler :: Handler, location :: Location, age :: Int}
  deriving (Eq)

instance Show Assignment where
  show (MkAssignment{..}) = "(" ++ show orangutan ++ ", " ++ show handler ++ ", " ++ show location ++ ", " ++ show age ++ ")"

type Solution = [Assignment]

answers :: [Solution]
answers = do
  solution@[merah, ofallo, quirrel, shamir] <-
    [ zipWith4 MkAssignment orangutans hs ls as
      | hs <- permutations handlers
      , ls <- permutations locations
      , as <- permutations ages
      ]

  guard $ shamir.age == 7
  guard $ shamir.location == Ambalat
  Just tarakan <- [find (\a -> a.location == Tarakan) solution]
  guard (quirrel.age < tarakan.age)

  let clue4 a1 a2 = a1.handler == Gracie && a2.age == 13
  guard $ clue4 ofallo tarakan || clue4 tarakan ofallo
  guard $ ofallo /= tarakan

  Just ambalat <- [find (\a -> a.location == Ambalat) solution]
  guard $ ambalat.age == 10 || ambalat.handler == Francine

  guard $ ofallo.age /= 10

  Just kendisi <- [find (\a -> a.location == Kendisi) solution]
  Just dolly <- [find (\a -> a.handler == Dolly) solution]
  guard $ kendisi.age > dolly.age

  return solution
