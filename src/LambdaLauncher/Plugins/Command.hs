module LambdaLauncher.Plugins.Command where

import LambdaLauncher.Types
import System.Process (callCommand, spawnCommand)
import System.Environment (getEnv)
import System.FilePath.Posix (splitSearchPath)
import System.Directory (doesDirectoryExist, listDirectory)
import Data.List (isInfixOf, isPrefixOf)
import Control.Monad (filterM, void)

command :: String -> IO [Result]
command s = do
    envs <- splitSearchPath <$> getEnv "PATH"
    existEnvs <- filterM doesDirectoryExist envs
    commandLists <- mapM listDirectory existEnvs
    let commands n = take n $ filter (s `isPrefixOf`) $ mconcat commandLists
    mapM result (commands 3)
  where
    result :: String -> IO Result
    result x = pure
      $ Action ("Run command " ++ max x s) 1
      $ void $ spawnCommand $ max s x
