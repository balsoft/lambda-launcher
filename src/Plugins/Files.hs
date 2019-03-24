module Plugins.Files where

import Types

import Plugins.Support (openUrlAction)

import System.Directory

files :: String -> IO [Result]
files s =
  map (\f -> Action ("Open file " ++ f) 3 $ openUrlAction f) <$> map (s ++) <$>
  listDirectory s
