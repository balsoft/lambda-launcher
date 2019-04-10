{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Emacs where

import Control.Monad (void)
import Data.List (isInfixOf)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Process (spawnCommand)
import LambdaLauncher.Types
import Data.Text (Text)

import qualified Data.Text as T


projectsDir :: IO FilePath
projectsDir = (++ "/projects/") <$> getEnv "HOME"

emacsOpenAction :: Text -> Result
emacsOpenAction s =
  Action (T.append "Open emacs in " s) 2 $ do
    dir <- projectsDir
    files <- listDirectory $ dir ++ T.unpack s
    void $
      if "default.nix" `elem` files
        then spawnCommand $ "cd " ++ dir ++ T.unpack s ++ "; nix-shell --run emacs"
        else spawnCommand $ "cd " ++ dir ++ T.unpack s ++ "; emacsclient -c ."

emacs :: Plugin
emacs s = fmap emacsOpenAction
  . filter (T.isInfixOf s)
  . map T.pack
  <$> (listDirectory =<< (++ "/projects") <$> getEnv "HOME")
