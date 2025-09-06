-- {-# LANGUAGE DeriveDataTypeable #-}
module Typeable where

import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import Type.Reflection
import GHC.Generics (Generic, Rep)

doSomethingSpecialOnInts :: (Typeable a) => a -> a
doSomethingSpecialOnInts x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @Int = x + 1
  | otherwise = x

data Wurble = MkW Int Bool Int Double
  deriving (Show, Data)

-- data tells us not only the left hand side but also the right hand side
add1 :: (Data a) => a -> a
add1 = everywhere (mkT ((+ 1) :: Int -> Int))

-- >>> add1 (MkW 2 True 3 8)
-- MkW 3 True 4 8.0

-- >>> :t everywhere
-- everywhere :: (forall a. Data a => a -> a) -> forall a. Data a => a -> a

data Record = MkR {field :: Int, other_field :: Bool}
  deriving Generic

-- >>> :kind! (Rep Record)
-- (Rep Record) :: * -> *
-- = M1
--     D
--     ('MetaData "Record" "Typeable" "puzzel-0.1.0.0-inplace" 'False)
--     (M1
--        C
--        ('MetaCons "MkR" 'PrefixI 'True)
--        (M1
--           S
--           ('MetaSel
--              ('Just "field")
--              'NoSourceUnpackedness
--              'NoSourceStrictness
--              'DecidedLazy)
--           (K1 R Int)
--         :*: M1
--               S
--               ('MetaSel
--                  ('Just "other_field")
--                  'NoSourceUnpackedness
--                  'NoSourceStrictness
--                  'DecidedLazy)
--               (K1 R Bool)))

data A = MkA (A -> Int)
