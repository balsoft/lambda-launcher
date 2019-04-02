module LambdaLauncher.Plugins.Files where

import LambdaLauncher.Types
import LambdaLauncher.Plugins.Support (openUrlAction)

import System.Directory

import qualified Data.Text as T

files :: Plugin
files s = map (((\f -> Action (T.append "Open file " f) Nothing 3 $ openUrlAction f) . T.append s) . T.pack)
  <$> listDirectory (T.unpack s)
