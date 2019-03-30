module LambdaLauncher.Plugins.Wmctrl where

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import LambdaLauncher.Types
import qualified Data.Text as T

wmctrl :: Plugin
wmctrl s = filter (T.isInfixOf s . shownText)
  . fmap ((\w -> Action (T.pack $ unwords $ drop 3 w) 2
  $ callProcess "wmctrl" ["-ia", head w]) . words)
  . lines
  <$> readProcess "wmctrl" ["-l"] ""
