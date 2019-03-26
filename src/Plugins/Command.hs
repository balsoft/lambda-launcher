module Plugins.Command where

import Types

import System.Process (callCommand, spawnCommand)

import System.Environment (getEnv)

import System.FilePath.Posix (splitSearchPath)

import System.Directory (doesDirectoryExist, listDirectory)

import Data.List (isInfixOf, isPrefixOf)

import Control.Monad (filterM, void)

command :: String -> IO [Result]
command s = do
  executables <-
    fmap mconcat <$> mapM listDirectory =<<
    filterM doesDirectoryExist =<< splitSearchPath <$> getEnv "PATH"
  let suitable =
        filter (\x -> (s `isInfixOf` x) || (x `isPrefixOf` s)) executables
  return $
    map
      (\x ->
         Action ("Run command " ++ max x s) 1 $ void $ spawnCommand $ max s x) $
    take 3 $ suitable
