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

removeSame :: Eq a => [a] -> [a] -> [a]
removeSame found (x:xs) =
  if x `elem` found
  then removeSame found xs
  else removeSame (x:found) xs
removeSame found _ = found

command :: Text -> IO [Result]
command query = do
    envs <- splitSearchPath <$> getEnv "PATH"
    existEnvs <- filterM doesDirectoryExist envs
    commandList <- mconcat <$> mapM listDirectory existEnvs
    let s = T.unpack query
    let cmd = head $ words s
    let args = ' ':(unwords $ tail $ words s)
    let commands =
          map T.pack $ fmap (++ args) $ simpleFilter cmd commandList
    mapM result $ take 3 $ removeSame [] commands
  where
    result :: Text -> IO Result
    result x = pure
      $ Action ("Run command " <>  x) 1
      $ void $ spawnCommand $ T.unpack x
