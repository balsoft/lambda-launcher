{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Qalc where

import LambdaLauncher.Plugins.Support (copyAction)
import System.Process (readProcess)
import LambdaLauncher.Types

import Data.Text (pack, unpack)


qalc :: Plugin
qalc s = sequence $ pure $
  copyAction 1 . pack <$>
  readProcess "qalc" ["-t", unpack s] mempty
