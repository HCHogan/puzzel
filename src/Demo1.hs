module Demo1 where

import Control.Monad.Reader (liftIO)
import Control.Monad.Reader qualified as R
import Control.Monad.Writer qualified as W

type MyShit a = W.WriterT [Int] (R.ReaderT Int IO) a

test :: MyShit ()
test = do
  h <- R.ask
  W.tell [h]
  liftIO $ print h

runMyShit :: MyShit a -> IO ()
runMyShit m = do
  let a = W.runWriterT m
  print "fuck"

newtype MyReaderT r m a = MyReaderT {runMyReaderT :: r -> m a}

instance (Functor m) => Functor (MyReaderT r m) where
  fmap f (MyReaderT r) = MyReaderT $ fmap (fmap f) r

instance (Applicative m) => Applicative (MyReaderT r m) where
  pure a = MyReaderT $ const $ pure a

  (<*>) (MyReaderT ab) (MyReaderT a) = MyReaderT $ \r ->
    let mab = ab r
        ma = a r
     in mab <*> ma

instance (Monad m) => Monad (MyReaderT r m) where
  (>>=) (MyReaderT rma) f = MyReaderT $ \r -> rma r >>= (\a -> let (MyReaderT rmb) = f a in rmb r)

class MyMonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a

instance (Monad m) => MyMonadReader r (MyReaderT r m) where
  ask = MyReaderT $ \r -> pure r
  local f (MyReaderT rma) = MyReaderT $ \r -> rma $ f r
