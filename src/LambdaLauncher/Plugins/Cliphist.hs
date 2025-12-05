{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaLauncher.Plugins.Cliphist where

import Data.Aeson
import Data.Maybe (maybeToList)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import LambdaLauncher.Types
import System.Process (readProcess)
import Control.Monad (void)
import qualified Text.Fuzzy as F

data ClipboardEntry = ClipboardEntry
  { number :: Int,
    text :: String
  }

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

instance Read ClipboardEntry where
  readsPrec _ input =
    let w = wordsWhen (== '\t') input
     in pure
          ( ClipboardEntry
              (read $ w !! 0)
              (w !! 1),
            ""
          )

listEntries :: IO [ClipboardEntry]
listEntries = fmap read . lines <$> readProcess "cliphist" ["list", "-preview-width", "58"] ""

copyEntry :: Int -> IO ()
copyEntry pid = void $ readProcess "cliphist" ["decode", show pid] "" >>= readProcess "wl-copy" []

cliphist :: Plugin
cliphist s = do
  entries <- listEntries
  pure $
    fmap
      (\ClipboardEntry {..} -> Action (pack ("📋 " ++ text)) 3 $ copyEntry number)
      (F.original <$> F.filter s entries "" "" (pack . text) False)
