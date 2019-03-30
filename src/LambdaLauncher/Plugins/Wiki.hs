{-# LANGUAGE DeriveGeneric #-}

module LambdaLauncher.Plugins.Wiki where

import LambdaLauncher.Types

import LambdaLauncher.Plugins.Support

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, putStrLn)
import Data.Default.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import System.Process (callProcess)

import qualified Data.Text as T

data Response = Response
  { query :: Query
  } deriving (Show, Generic)

data Query = Query
  { search :: [SearchResult]
  } deriving (Show, Generic)

data SearchResult = SearchResult
  { title :: String
  , snippet :: String
  , pageid :: Integer
  } deriving (Show, Generic)

instance FromJSON SearchResult

instance FromJSON Query

instance FromJSON Response

next :: Char -> (String, Integer) -> (String, Integer)
next '<' (s, i) = (s, i + 1)
next '>' (s, i) = (s, i - 1)
next chr (s, i) =
  if i == 0
    then (chr : s, i)
    else (s, i)

removeTags :: String -> String
removeTags = fst . foldr next ("", 0)

replaceAmps :: String -> String
replaceAmps [] = ""
replaceAmps ('&':'q':'u':'o':'t':';':xs) = '"' : replaceAmps xs
replaceAmps ('&':'l':'t':';':xs) = '<' : replaceAmps xs
replaceAmps ('&':'g':'t':';':xs) = '>' : replaceAmps xs
replaceAmps ('&':'a':'m':'p':xs) = '&' : replaceAmps xs
replaceAmps ('&':_:'d':'a':'s':'h':';':xs) = " - " ++ replaceAmps xs
replaceAmps (c:xs) = c : replaceAmps xs

htmlToPlain :: String -> String
htmlToPlain = replaceAmps . removeTags

getWikipediaData :: Text -> IO ByteString
getWikipediaData s =
  runReq def $ do
    bs <-
      req GET (https "wikipedia.org" /: "w" /: "api.php") NoReqBody bsResponse $
      "action" =: ("query" :: String) <> "list" =: ("search" :: String) <>
      "srsearch" =:
      s <>
      "srlimit" =:
      (5 :: Integer) <>
      "format" =:
      ("json" :: String)
    liftIO $ return $ responseBody bs

wiki :: Plugin
wiki s = do
  (Just response) <- decodeStrict <$> getWikipediaData s
  let s = search $ query response
  let res = (\x -> (T.pack $ htmlToPlain $ snippet x, T.pack . show $ pageid x)) <$> s
  return $
    (\(text, curid) ->
       Action text 4 $
       openUrlAction $ T.append "https://en.wikipedia.org/?curid=" curid) <$> res
