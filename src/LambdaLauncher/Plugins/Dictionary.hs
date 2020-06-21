{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Dictionary where

import LambdaLauncher.Types

import LambdaLauncher.Plugins.Support

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Data.Maybe (maybeToList)


data Response = Response
  { word :: Text
  , phonetic :: Text
  , origin :: Text
  , meaning :: Meaning
  } deriving (Show, Generic)

data Meaning = Meaning
  { exclamation :: Maybe [Definition]
  , noun :: Maybe [Definition]
  , verb :: Maybe [Definition]
  , adverb :: Maybe [Definition]
  , adjective :: Maybe [Definition]
  } deriving (Show, Generic)

data Definition = Definition
  { definition :: Text
  , example :: Maybe Text
  , synonyms :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON Response
instance FromJSON Meaning
instance FromJSON Definition

getDictionaryData :: Text -> IO ByteString
getDictionaryData s =
  runReq defaultHttpConfig $ do
    bs <-
      req GET (https "googledictionaryapi.eu-gb.mybluemix.net") NoReqBody bsResponse $
      "lang" =: ("en" :: Text) <>
      "define" =: s
    liftIO $ return $ responseBody bs

getResults :: Response -> [LambdaLauncher.Types.Result]
getResults r = mkResult <$> texts
  where
    mkResult = copyAction 2 
    texts = definition <$> definitions
    definitions = mconcat $ maybeToList $ (noun <> verb <> adverb <> adjective <> exclamation) m
    m = meaning r

dictionary :: Plugin
dictionary q = mconcat . mconcat . maybeToList . (fmap (map getResults)) <$> decodeStrict  <$> getDictionaryData q
