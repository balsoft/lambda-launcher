module LambdaLauncher.Plugins.Google where

import LambdaLauncher.Types

import LambdaLauncher.Plugins.Support

google :: Plugin
google s =
  return $
  pure $
  Action ("Google " ++ s) 2 $
  openUrlAction $ "https://google.com/search?q=" ++ s
