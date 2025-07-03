module TIT where

import Data.Kind

type App :: forall {k}. (k -> Type) -> k -> Type
data App f a = MkApp (f a)

-- >>> :kind App
-- App :: (k -> *) -> k -> *

