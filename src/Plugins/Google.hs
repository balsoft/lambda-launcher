module Plugins.Google where

import Types

import Plugins.Support

google :: Plugin
google s =
  return $
  pure $
  Action ("Google " ++ s) 2 $
  openUrlAction $ "https://google.com/search?q=" ++ s
