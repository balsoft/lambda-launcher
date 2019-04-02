module LambdaLauncher.Plugins.Duckduckgo where

import LambdaLauncher.Plugins.Support
import LambdaLauncher.Types

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Default.Class
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req
import GI.Gtk (Image, imageNewFromFile)
import Data.ByteString (writeFile)

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
  runReq def $ do
    bs <-
      req GET (https "api.duckduckgo.com") NoReqBody bsResponse $
      ("q" =: s) <>
      ("format" =: ("json" :: String))
    liftIO $ return $ responseBody bs

fetchFavicon :: IO String
fetchFavicon = do
  Data.ByteString.writeFile file
    =<< (runReq def $ do
            bs <-
              req GET (https "duckduckgo.com" /: "favicon.ico") NoReqBody bsResponse mempty
            liftIO $ return $ responseBody bs)
  return file
  where file = "/tmp/ddg.ico"
  
getDDGInstantResponse :: Text -> IO DDGResponse
getDDGInstantResponse s = do
  Right b <- eitherDecodeStrict <$> fetchDDGAPI s
  return b

topicToResult :: Image -> RelatedTopic -> Maybe LambdaLauncher.Types.Result
topicToResult i (RelatedTopic (Just t) (Just u)) =
  Just $ Action t (Just i) 2 $ openUrlAction u
topicToResult _ _ = Nothing

duckduckgo :: Plugin
duckduckgo s | s == "" = mempty
             | otherwise = do
                 imageFile <- fetchFavicon
                 image <- imageNewFromFile imageFile
                 DDGResponse r <- getDDGInstantResponse s
                 return $ catMaybes $ (topicToResult image) <$> r
