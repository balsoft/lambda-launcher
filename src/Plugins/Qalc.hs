{-# LANGUAGE OverloadedStrings #-}

module Plugins.Qalc where

import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Plugins.Support (copyAction)
import System.Process (readProcess)
import Types

qalc :: String -> IO [Result]
qalc s = do
  str <- readProcess "qalc" [] s
  let res =
        Prelude.head . reverse . map Text.unpack . Text.splitOn "=" . Text.pack <$>
        (listToMaybe $ take 1 $ reverse $ take 3 $ lines str)
  return $
    concat $
    filter (\(Action x _) -> not $ "error" `isInfixOf` x) . pure . copyAction <$>
    (res :: Maybe String)
