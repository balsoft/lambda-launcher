module LambdaLauncher.Plugins.Google where

import LambdaLauncher.Types
import LambdaLauncher.Plugins.Support

import qualified Data.Text as T

google :: Plugin
google s =
  return $
  pure $
  Action (T.append "Google " s) 2 $
  openUrlAction $ T.append "https://google.com/search?q=" s
