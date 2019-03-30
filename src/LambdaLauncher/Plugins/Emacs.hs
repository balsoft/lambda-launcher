module LambdaLauncher.Plugins.Emacs where

import Control.Monad (void)
import Data.List (isInfixOf)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.Process (spawnCommand)
import LambdaLauncher.Types

projectsDir :: IO FilePath
projectsDir = (++ "/projects/") <$> getEnv "HOME"

emacsOpenAction :: String -> Result
emacsOpenAction s =
  Action ("Open emacs in " ++ s) 2 $ do
    dir <- projectsDir
    files <- listDirectory $ dir ++ s
    void $
      if "default.nix" `elem` files
        then spawnCommand $ "cd " ++ dir ++ s ++ "; nix-shell --run emacs"
        else spawnCommand $ "cd " ++ dir ++ s ++ "; emacsclient -c ."

emacs :: Plugin
emacs s =
  fmap emacsOpenAction <$> filter (s `isInfixOf`) <$>
  (listDirectory =<< (++ "/projects") <$> getEnv "HOME")
