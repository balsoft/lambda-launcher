{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Duckduckgo where

import LambdaLauncher.Plugins.Support
import LambdaLauncher.Types

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Default.Class
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req

data RelatedTopic = RelatedTopic
  { text :: Maybe Text
  , firstURL :: Maybe Text
  } deriving (Eq, Show)

data DDGResponse = DDGResponse
  { relatedTopics :: [RelatedTopic]
  } deriving (Eq, Show)

instance FromJSON RelatedTopic where
  parseJSON (Object o) = RelatedTopic <$> (o .:? "Text") <*> (o .:? "FirstURL")
  parseJSON _ = mempty

instance FromJSON DDGResponse where
  parseJSON (Object o) = DDGResponse <$> o .: "RelatedTopics"
  parseJSON _ = mempty
  

fetchDDGAPI s =
  runReq defaultHttpConfig $ do
    bs <-
      req GET (https "api.duckduckgo.com") NoReqBody bsResponse $
      ("q" =: s) <>
      ("format" =: ("json" :: String))
    liftIO $ return $ responseBody bs

getDDGInstantResponse :: Text -> IO DDGResponse
getDDGInstantResponse s = do
  Right b <- eitherDecodeStrict <$> fetchDDGAPI s
  return b

topicToResult :: RelatedTopic -> Maybe LambdaLauncher.Types.Result
topicToResult (RelatedTopic (Just t) (Just u)) =
  Just $ Action t 2 $ openUrlAction u
topicToResult _ = Nothing

duckduckgo :: Plugin
duckduckgo s | s == "" = mempty
             | otherwise = do
                 DDGResponse r <- getDDGInstantResponse s
                 return $ catMaybes $ topicToResult <$> r
