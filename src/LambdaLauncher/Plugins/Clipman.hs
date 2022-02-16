{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Clipman where

import LambdaLauncher.Plugins.Support (copyAction)
import System.Process (readProcess)
import LambdaLauncher.Types

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)

import Data.Aeson

import Data.Maybe (maybeToList)


clipman :: Plugin
clipman s = do
  historyJson <- encodeUtf8 . pack <$> readProcess "clipman" ["show-history"] mempty
  let history = concat $ maybeToList $ decodeStrict historyJson
  pure $ map (copyAction 1) history
