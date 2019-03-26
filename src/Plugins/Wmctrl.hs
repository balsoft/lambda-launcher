module Plugins.Wmctrl where

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import Types

wmctrl s =
  filter (\(Action t _ _) -> s `isInfixOf` t) <$>
  fmap
    (\w -> Action (unwords $ drop 3 w) 2 $ callProcess "wmctrl" ["-ia", w !! 0]) <$>
  fmap words <$>
  lines <$>
  readProcess "wmctrl" ["-l"] ""
