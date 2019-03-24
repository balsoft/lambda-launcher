module Plugins.Command where

import Types

import System.Process (callCommand)

import System.Environment (getEnv)

import System.FilePath.Posix (splitSearchPath)

import System.Directory (doesDirectoryExist, listDirectory)

import Data.List (isInfixOf, isPrefixOf)

import Control.Monad (filterM)

command :: String -> IO [Result]
command s = do
  path <- splitSearchPath <$> getEnv "PATH"
  pathExists <- filterM (doesDirectoryExist) path
  executables <- concat <$> (sequence $ listDirectory <$> pathExists)
  let suitable =
        filter (\x -> (s `isInfixOf` x) || (x `isPrefixOf` s)) executables
  return $
    map (\x -> Action ("Run command " ++ max x s) $ callCommand $ max s x) $
    take 5 $ suitable
