module Recursive where

newtype Hungry = Hungry (Int -> Hungry)

f :: Hungry
f = Hungry $ const f

newtype Stream = Stream (() -> (Int, Stream))

hd :: Stream -> Int
hd (Stream s) = let (x, _) = s () in x

tl :: Stream -> Stream
tl (Stream s) = let (_, s') = s () in s'

upfrom0 :: Stream
upfrom0 = upfrom0' 0
  where
    upfrom0' :: Int -> Stream
    upfrom0' n = Stream $ \() -> (n, upfrom0' (n + 1))

newtype Process = Process (Int -> (Int, Process))

p :: Process
p = p' 0
  where
    p' :: Int -> Process
    p' n = Process $ \acc -> let newacc = acc + n in (newacc, p' newacc)

damn = 1 + 5

curr :: Process -> Int
curr (Process f) = let (x, _) = f 0 in x

send :: Int -> Process -> Process
send n (Process f) = let (_, p') = f n in p'

a = 1 + 1

newtype D = D {unD :: D -> D}

y :: D
y = D $ \f ->
  let g = D $ \x ->
        unD f (unD x x)
   in unD g g
