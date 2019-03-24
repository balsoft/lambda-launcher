module Plugins.Google where

import Types

import System.Process (callProcess)

google :: String -> IO [Result]
google s =
  return $
  pure $
  Action ("Google " ++ s) $
  callProcess "xdg-open" ["https://google.com/?q=" ++ s]
