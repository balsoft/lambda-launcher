module LambdaLauncher.Plugins.Files where

import LambdaLauncher.Types

import LambdaLauncher.Plugins.Support (openUrlAction)

import System.Directory

files :: Plugin
files s =
  map ((\f -> Action ("Open file " ++ f) 3 $ openUrlAction f) . (s ++)) <$>
  listDirectory s
