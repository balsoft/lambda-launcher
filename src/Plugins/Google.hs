module Plugins.Google where

import Types

import Plugins.Support

import System.Process (callProcess)

google :: String -> IO [Result]
google s =
  return $
  pure $
  Action ("Google " ++ s) 2 $ openUrlAction $ "https://google.com/?q=" ++ s
