{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Command where

import LambdaLauncher.Types
import System.Process (spawnCommand)
import System.Environment (getEnv)
import System.FilePath.Posix (splitSearchPath)
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Fuzzy (simpleFilter)
import Data.List (intersperse)
import Control.Monad (filterM, void)
import Data.Text (Text)

import qualified Data.Text as T

command :: Plugin
command query = do
    envs <- splitSearchPath <$> getEnv "PATH"
    existEnvs <- filterM doesDirectoryExist envs
    commandList <- mconcat <$> mapM listDirectory existEnvs
    let s = T.unpack query
    let cmd = head $ words s
    let args = ' ':unwords (tail $ words s)
    let commands =
          map T.pack $ fmap (++ args) $ simpleFilter cmd $ filter ((/= '.') . head) commandList
    mapM result commands
  where
    result :: Text -> IO Result
    result x = pure
      $ Action ("Run command " <>  x) 1
      $ void $ spawnCommand $ T.unpack x
