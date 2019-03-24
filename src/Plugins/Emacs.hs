module Plugins.Emacs where

import Data.List (isInfixOf)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Process (spawnCommand)
import Types

projectsDir :: IO FilePath
projectsDir = (++ "/projects/") <$> getEnv "HOME"

emacsOpenAction :: String -> Result
emacsOpenAction s =
  Action ("Open emacs in " ++ s) 2 $ do
    dir <- projectsDir
    files <- listDirectory =<< (++ s) <$> projectsDir
    _ <-
      if "default.nix" `elem` files
        then spawnCommand $ "cd " ++ dir ++ s ++ "; nix-shell --run emacs"
        else spawnCommand $ "cd " ++ dir ++ s ++ "; emacsclient -c ."
    return ()

emacs :: String -> IO [Result]
emacs s =
  fmap emacsOpenAction <$> filter (s `isInfixOf`) <$>
  (listDirectory =<< (++ "/projects") <$> getEnv "HOME")
