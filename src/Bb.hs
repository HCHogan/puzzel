{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Bb () where

import Data.List (unfoldr)

data Inst
  = Label String
  | Op String
  | Term String
  deriving (Show, Eq)

isLabel, isTerm :: Inst -> Bool
isLabel (Label _) = True
isLabel _ = False
isTerm (Term _) = True
isTerm _ = False

extractBlock :: [Inst] -> ([Inst], [Inst])
extractBlock [] = ([], [])
extractBlock (i : is)
  | isTerm i = ([i], is)
  | otherwise = case is of
      (nextI : _) | isLabel nextI -> ([i], is)
      _ ->
        let (block, rest) = extractBlock is
         in (i : block, is)

fromBlocks :: [Inst] -> [[Inst]]
fromBlocks = unfoldr \case
  [] -> Nothing
  xs -> Just (extractBlock xs)
