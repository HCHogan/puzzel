{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Pattern where

data L a = N | C a (L a)
  deriving (Show)

toVCons :: Show a => L a -> Maybe (String, L a)
toVCons N          = Nothing
toVCons (C x rest) = Just (show x, rest)

pattern VNil  :: L a
pattern VNil <- N

pattern VCons :: Show a => String -> L a -> L a
pattern VCons s xs <- (toVCons -> Just (s, xs))

f :: Maybe (L Int) -> IO ()
f Nothing             = putStrLn "none"
f (Just VNil)         = putStrLn "nil"
f (Just (C i N))      = print i
f (Just (VCons s xs)) = putStrLn s *> f (Just xs)

main :: IO ()
main = do
  let l0 = N
      l1 = C 42 N
      l2 = C 1 (C 2 (C 3 N))
  mapM_ (f . Just) [l0, l1, l2]
  f Nothing
