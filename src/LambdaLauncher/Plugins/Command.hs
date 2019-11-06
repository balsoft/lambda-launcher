{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Command where

import LambdaLauncher.Types
import System.Process (spawnCommand)
import System.Environment (getEnv)
import System.FilePath.Posix (splitSearchPath)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Fuzzy (simpleFilter)
import Control.Monad (filterM, void)
import Data.Text (Text)

import qualified Data.Text as T

command :: Text -> IO [Result]
command query = do
    envs <- splitSearchPath <$> getEnv "PATH"
    existEnvs <- filterM doesDirectoryExist envs
    commandList <- mconcat <$> mapM listDirectory existEnvs
    let s = T.unpack query
    let command = head $ words s
    let commands =
          map T.pack $ (++ tail s) <$> simpleFilter command commandList
    mapM result $ take 3 commands
  where
    result :: Text -> IO Result
    result x = pure
      $ Action (T.append "Run command " $ max x query) 1
      $ void $ spawnCommand $ T.unpack $ max query x
