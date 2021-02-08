{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module LambdaLauncher.Plugins.UrbanDictionary where

import LambdaLauncher.Plugins.Support
import LambdaLauncher.Types

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Default.Class
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req
import GHC.Generics

data Definition = Definition
  { definition :: Text
  , word :: Text
  , author :: Text
  , permalink :: Text
  , defid :: Int
  , thumbs_up :: Int
  , thumbs_down :: Int
  , example :: Text
  } deriving (Eq, Show, Generic)

data Definitions = Definitions
  { list :: [Definition]
  } deriving (Eq, Show, Generic)


instance FromJSON Definition

instance FromJSON Definitions

fetchUBDefinitions s =
  runReq defaultHttpConfig $ do
    bs <-
      req GET (https "api.urbandictionary.com" /: "v0" /: "define") NoReqBody bsResponse $ ("term" =: s)
    liftIO $ return $ responseBody bs

defToResult :: Definition -> LambdaLauncher.Types.Result
defToResult (Definition {..}) = Action (word<>": "<>definition) 2 $ openUrlAction permalink

urbanDictionary :: Plugin
urbanDictionary s | s == "" = mempty
                  | otherwise = do
                      Right(Definitions r) <- eitherDecodeStrict <$> fetchUBDefinitions s
                      return $ defToResult <$> r
