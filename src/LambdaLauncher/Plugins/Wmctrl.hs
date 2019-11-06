{-# LANGUAGE OverloadedStrings #-}
module LambdaLauncher.Plugins.Wmctrl where

import System.Process (callProcess, readProcess)
import LambdaLauncher.Types
import qualified Data.Text as T
import Text.Fuzzy (simpleFilter)

toResult :: T.Text -> Result
toResult s =
  Action
  ("Switch to " <> (T.unwords $ drop 3 $ T.words s)) 2
  $ callProcess "wmctrl" ["-ia", T.unpack $ head $ T.words s]

wmctrl :: Plugin
wmctrl s =
  (fmap toResult
   . simpleFilter s 
   . T.lines
   . T.pack)
  <$> readProcess "wmctrl" ["-l"] ""
