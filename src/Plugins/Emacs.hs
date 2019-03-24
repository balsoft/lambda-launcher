module Plugins.Emacs where

import Data.List (isInfixOf)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Process (callCommand)
import Types

projectsDir = (++ "/projects/") <$> getEnv "HOME"

emacsOpenAction :: String -> Result
emacsOpenAction s =
  Action ("Open emacs in " ++ s) $ do
    files <- listDirectory =<< (++ s) <$> projectsDir
    if "default.nix" `elem` files
      then callCommand $ "cd " ++ s ++ "; nix-shell --run emacs"
      else callCommand $ "cd " ++ s ++ "; emacsclient -c ."

emacs :: String -> IO [Result]
emacs s =
  fmap emacsOpenAction <$> filter (s `isInfixOf`) <$>
  (listDirectory =<< (++ "/projects") <$> getEnv "HOME")
