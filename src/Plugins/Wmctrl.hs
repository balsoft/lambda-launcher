module Plugins.Wmctrl where

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import Types

wmctrl s =
  filter (\(Action t _ _) -> s `isInfixOf` t) <$>
  fmap
    (\w -> Action (unwords $ drop 2 w) 4 $ callProcess "wmctrl" ["-ia", w !! 0]) <$>
  fmap words <$>
  lines <$>
  readProcess "wmctrl" ["-l"] ""
