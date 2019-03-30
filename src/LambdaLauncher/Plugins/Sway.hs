{-# LANGUAGE DeriveGeneric #-}

module LambdaLauncher.Plugins.Sway where

import Data.Aeson
import Data.List (isInfixOf)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import System.Process (callProcess, readProcess)
import LambdaLauncher.Types

import qualified Data.Text as T


data Node = Node
  { nName :: Maybe Text
  , nApp_id :: Maybe Text
  , nId :: Integer
  , nPid :: Maybe Integer
  , nNodes :: Maybe [Node]
  } deriving (Show, Generic)

data Window = Window
  { wName   :: Text
  , wApp_id :: Maybe Text
  , wId     :: Integer
  , wPid    :: Integer
  }

instance FromJSON Node

instance ToJSON Node

findWindows :: Node -> [Window]
findWindows (Node (Just name) nApp_id nId (Just pid) _) =
  [Window name nApp_id nId pid]
findWindows node = concat (mconcat . map findWindows <$> nNodes node)

windowToResults :: Window -> LambdaLauncher.Types.Result
windowToResults Window{..} =
  Action wName 2 $ callProcess "swaymsg" ["[con_id=" ++ show wId ++ "] focus"]

sway :: Plugin
sway s = do
  tree <- encodeUtf8 . pack <$> readProcess "swaymsg" ["-t", "get_tree"] ""
  pure $ concat $
    map windowToResults
      . filter (\Window{..} -> s `T.isInfixOf` wName)
      . findWindows
      <$> decodeStrict tree
