{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.GoogleTranslate (googletranslate) where

import LambdaLauncher.Types (Plugin)
import LambdaLauncher.Plugins.Support

import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Default.Class
import Data.Monoid ((<>))
import Prelude hiding (words, concat)
import Data.Text (Text, words)
import Data.List (intersperse)
import Network.HTTP.Req
import Data.Vector ((!?))

instance FromJSON GoogleTranslateResult where
  parseJSON j = do
    Array x <- parseJSON j
    Array y <- parseJSON $ Data.Maybe.fromMaybe (Array mempty) $ x !? 0
    Array z <- parseJSON $ Data.Maybe.fromMaybe (Array mempty) $ y !? 0
    String text <- parseJSON $ Data.Maybe.fromMaybe (String mempty) $ z !? 0
    pure (GoogleTranslateResult text)

newtype GoogleTranslateResult = GoogleTranslateResult {getText :: Text}

translate :: Text -> Text -> Text -> IO (Maybe GoogleTranslateResult)
translate from to text =
  decodeStrict <$>
  runReq defaultHttpConfig (do
  bs <-
    req GET (https "translate.googleapis.com" /: "translate_a" /: "single") NoReqBody bsResponse $
      "client" =: ("gtx" :: String)
    <>"sl" =: from
    <>"tl" =: to
    <>"dt" =: ("t" :: String)
    <>"ie" =: ("UTF-8" :: String)
    <>"oe" =: ("UTF-8" :: String)
    <>"q" =: text
  liftIO $ pure $ responseBody bs)

googletranslate :: Plugin
googletranslate s
  | length w >= 3 =
    fmap (copyAction 1 . getText) . maybeToList <$>
    translate (head w) (w !! 1) (mconcat $ intersperse " " $ drop 2 w)
  | otherwise = mempty
  where w = words s

