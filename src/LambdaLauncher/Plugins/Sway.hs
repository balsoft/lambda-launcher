{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaLauncher.Plugins.Sway where

import Data.Aeson
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import System.Process (callProcess, readProcess)
import LambdaLauncher.Types
import qualified Text.Fuzzy as F

import qualified Data.Text as T


data Node = Node
  { name :: Maybe Text
  , app_id :: Maybe Text
  , id :: Integer
  , pid :: Maybe Integer
  , nodes :: Maybe [Node]
  } deriving (Show, Generic)

data Window = Window
  { wName   :: Text
  , wApp_id :: Maybe Text
  , wId     :: Integer
  , wPid    :: Integer
  } deriving (Show)

instance FromJSON Node

instance ToJSON Node

-- | Find windows in a tree starting from Node
findWindows :: Node -> [Window]
-- If this node is a window, it must have a name and PID associated with it
findWindows (Node (Just name) nApp_id nId (Just pid) _) =
  [Window name nApp_id nId pid]
-- Otherwise, look at node's children recursively
findWindows node = concatMap (mconcat . map findWindows) (nodes node)

windowToResults :: Window -> LambdaLauncher.Types.Result
windowToResults Window{..} =
  Action ("Switch to " <> wName) 2 $ callProcess "swaymsg" ["[con_id=" ++ show wId ++ "] focus"]

sway :: Plugin
sway s = do
  tree <- encodeUtf8 . pack <$> readProcess "swaymsg" ["-t", "get_tree"] ""
  let windows = concat $ maybeToList $ findWindows <$> decodeStrict tree
  let matchingWindows = F.original <$> F.filter s windows "" "" wName False
  pure $ map windowToResults matchingWindows
