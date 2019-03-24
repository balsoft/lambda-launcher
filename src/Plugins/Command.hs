module Plugins.Command where

import Types

import System.Process (spawnCommand)

import System.Environment (getEnv)

import System.FilePath.Posix (splitSearchPath)

import System.Directory (doesDirectoryExist, listDirectory)

import Data.List (isInfixOf, isPrefixOf)

import Control.Monad (filterM)

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
         Action ("Run command " ++ max x s) 1 $
         seq (spawnCommand $ max s x) (return ())) $
    take 3 $ suitable
