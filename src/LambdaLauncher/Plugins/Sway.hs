{-# LANGUAGE DeriveGeneric #-}

module LambdaLauncher.Plugins.Sway where

import Data.Aeson
import Data.List (isInfixOf)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import System.Process (callProcess, readProcess)
import LambdaLauncher.Types

data Node = Node
  { name :: Maybe String
  , app_id :: Maybe String
  , id :: Integer
  , pid :: Maybe Integer
  , nodes :: Maybe [Node]
  } deriving (Show, Generic)

data Window =
  Window String
         (Maybe String)
         Integer
         Integer

instance FromJSON Node

instance ToJSON Node

findWindows :: Node -> [Window]
findWindows ((Node (Just name) app_id id (Just pid) _)) =
  [Window name app_id id pid]
findWindows (node) = concat (mconcat <$> map findWindows <$> nodes node)

windowToResults :: Window -> LambdaLauncher.Types.Result
windowToResults (Window name app_id id pid) =
  Action name 2 $ callProcess "swaymsg" ["[con_id=" ++ show id ++ "] focus"]

sway :: Plugin
sway s = do
  tree <- encodeUtf8 <$> pack <$> readProcess "swaymsg" ["-t", "get_tree"] ""
  return $
    concat $
    map windowToResults <$> filter (\(Window name _ _ _) -> isInfixOf s name) <$>
    findWindows <$>
    decodeStrict tree
