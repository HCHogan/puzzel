module TagF where

import Control.Monad.Free

-- Free Monads

data MiniIOF a
  = Terminate
  | PrintLine String a
  | ReadLine (String -> a)
  deriving (Functor)

type MiniIOM = Free MiniIOF

terminateM :: MiniIOM a
terminateM = liftF Terminate

printlineM :: String -> MiniIOM ()
printlineM str = liftF (PrintLine str ())

readlineM :: MiniIOM String
readlineM = liftF (ReadLine id)

myProgramM :: MiniIOM ()
myProgramM = do
  printlineM "fuck"
  input <- readlineM
  printlineM $ "hello" ++ input
  terminateM

runInIO :: MiniIOM a -> IO a
runInIO (Pure a) = pure a
runInIO (Free m) = case m of
  Terminate -> error "shit"
  (PrintLine str k) -> do
    print str
    runInIO k
  (ReadLine f) -> do
    input <- getLine
    runInIO $ f input

mainM :: IO ()
mainM = runInIO myProgramM

-- Tagless final

class (Monad m) => MiniIOC m where
  terminateC :: m a
  readlineC :: m String
  printlineC :: String -> m ()

myProgramC :: (MiniIOC m) => m ()
myProgramC = do
  printlineC "fuck"
  input <- readlineC
  printlineC $ "hello" ++ input

instance MiniIOC IO where
  printlineC = putStrLn
  readlineC = getLine
  terminateC = pure $ error "fuck"

mainC :: IO ()
mainC = myProgramC @IO
