{-# LANGUAGE TypeFamilies #-}

module EffD where

import Data.Map.Strict qualified as M
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.State.Static.Local
import System.Console.Haskeline
import System.IO qualified as IO

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

readFile :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es String
readFile = send . ReadFile

writeFile :: (HasCallStack, FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile path content = send (WriteFile path content)

newtype FsError = FsError String deriving (Show)

runFileSystemIO :: (IOE :> es, Error FsError :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
 where
  adapt m = liftIO m `catchIO` \e -> throwError . FsError $ show e

runFileSystemPure ::
  (IOE :> es, Error FsError :> es) =>
  M.Map FilePath String -> Eff (FileSystem : es) a -> Eff es a
runFileSystemPure fs = reinterpret (evalState fs) $ \_ -> \case
  ReadFile path ->
    gets (M.lookup path) >>= \case
      Just contents -> pure contents
      Nothing -> throwError . FsError $ "File not found: " ++ path
  WriteFile path contents -> modify $ M.insert path contents

data Haskeline :: Effect where
