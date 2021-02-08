{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Pass where

import System.FilePath.Posix (takeBaseName)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import System.Process (callProcess)
import System.Environment (getEnv)
import LambdaLauncher.Types
import Control.Exception.Safe (catch, throwIO, try)
import System.IO.Error (isDoesNotExistError)
import Data.List (isSuffixOf)
import Control.Monad (void)

import System.Directory.Tree (DirTree(..), AnchoredDirTree(..), build)

import qualified Text.Fuzzy as F

import qualified Data.Text as T

getFilesByExtension :: String -> DirTree FilePath -> [FilePath]
getFilesByExtension ext Dir {..} = mconcat $ fmap (getFilesByExtension ext) contents
getFilesByExtension ext File {..} = if ext `isSuffixOf` name then pure file else []
getFilesByExtension _ _ = []

fileToSecret :: FilePath -> FilePath -> String
fileToSecret dir file = drop (length dir + 1) $ take (length file - 4) file

secretToResults :: String -> Result
secretToResults secret =
  Action
  { shownText = pack $ "Unlock "<>secret
  , priority = 1
  , action = void $ callProcess "pass" ["-c", secret]
  }

pass :: Plugin
pass s = do
  password_store <- getEnv "PASSWORD_STORE" `catch` \e ->
    if isDoesNotExistError e
    then getEnv "HOME" <> pure "/.password-store"
    else throwIO e
  tree <- build password_store
  let files = getFilesByExtension ".gpg" $ dirTree tree
  let secrets = fileToSecret password_store <$> files
  let matching = F.original <$> F.filter s secrets "" "" pack False
  return $ secretToResults <$> matching
