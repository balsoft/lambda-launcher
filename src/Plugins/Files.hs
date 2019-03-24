module Plugins.Files where

import Types

import Plugins.Support (openUrlAction)

import System.Directory

files :: String -> IO [Result]
files s = map openUrlAction <$> map (s ++) <$> listDirectory s
