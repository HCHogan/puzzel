module Cont where

import Control.Monad.Cont

addCont :: Int -> Int -> Cont r Int
addCont x y = return (x + y)

square :: Int -> Cont r Int
square x = return (x * x)

squareCCC :: Int -> Cont r Int
squareCCC x = callCC $ \k -> k (x * x)

testUndelimited :: Cont String Int
testUndelimited = do
  let x = 3
  let y = 4
  callCC $ \exit -> do
    -- exit here captures the return 111
    a <- cont $ \k ->
      let x = 3; y = 4
       -- in k (x + y)
       in "Shit"
    -- k here captures the following two lines of code
    exit a -- return a continuation ignores the return 999 and directly exits the callcc block with a
    return 999
  return 111

-- >>> let a = runCont testUndelimited show

-- 如果把无界连续性的 callCC 看成：
-- … 外层代码 …
-- callCC (\exit -> …)
-- … 外层剩余 …
--
-- 那么定界连续性相当于：
-- … 外层代码 …
-- prompt $ \() ->
-- … 中间若干代码 …
-- control (\capturedSegment -> …)
-- … 这之后的代码 属于 prompt 里的“捕获域” …
-- … 这里是 prompt 之外，不会被 control 捕获 …
