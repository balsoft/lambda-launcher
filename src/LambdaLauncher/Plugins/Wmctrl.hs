module LambdaLauncher.Plugins.Wmctrl where

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import LambdaLauncher.Types

wmctrl :: Plugin
wmctrl s = filter (isInfixOf s . shownText)
  . fmap ((\w -> Action (unwords $ drop 3 w) 2
  $ callProcess "wmctrl" ["-ia", w !! 0]) . words)
  . lines
  <$> readProcess "wmctrl" ["-l"] ""
