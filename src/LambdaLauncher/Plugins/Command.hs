{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Command where

import LambdaLauncher.Types
import System.Process (callCommand, spawnCommand)
import System.Environment (getEnv)
import System.FilePath.Posix (splitSearchPath)
import System.Directory (doesDirectoryExist, listDirectory)
import Data.List (isInfixOf, isPrefixOf)
import Control.Monad (filterM, void)
import Data.Text (Text)

import qualified Data.Text as T


command :: Text -> IO [Result]
command s = do
    envs <- splitSearchPath <$> getEnv "PATH"
    existEnvs <- filterM doesDirectoryExist envs
    commandList <- mconcat <$> mapM listDirectory existEnvs
    let commands n = map T.pack . take n
                     $ filter (\command -> (T.unpack s) `isPrefixOf` command
                                        || command `isPrefixOf` (T.unpack s))
                     commandList
    mapM result (commands 3)
  where
    result :: Text -> IO Result
    result x = pure
      $ Action (T.append "Run command " $ max x s) 1
      $ void $ spawnCommand $ T.unpack $ max s x
