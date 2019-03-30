{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}

module LambdaLauncher.Plugins.Stackoverflow where

import LambdaLauncher.Types

import LambdaLauncher.Plugins.Support

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Req

data Response = Response
  { items :: [Question]
  } deriving (Show, Generic)

data Question = Question
  { title :: String
  , question_id :: Integer
  } deriving (Show, Generic)

instance FromJSON Response

instance FromJSON Question

findSOQuestions :: String -> IO [Question]
findSOQuestions s =
  concat <$> maybeToList <$> fmap items <$> decodeStrict <$>
  ((runReq def $ do
      bs <-
        req
          GET
          (https "api.stackexchange.com" /: "2.2" /: "similar")
          NoReqBody
          bsResponse $
        "pagesize" =: (5 :: Integer) <>
        "order" =: ("desc" :: String) <>
        "sort" =: ("activity" :: String) <>
        "title" =: s <>
        "site" =: ("stackoverflow" :: String) <>
        "filter" =: ("!C(o*VkSJu.8H6s2yB" :: String)
      liftIO $ return $ responseBody bs) :: IO ByteString)

stackoverflow :: Plugin
stackoverflow s =
  fmap
    (\(Question t i) ->
       Action t 5 $ openUrlAction $ "https://stackoverflow.com/q/" ++ show i) <$>
  findSOQuestions s
