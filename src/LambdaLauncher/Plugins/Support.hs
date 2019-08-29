module LambdaLauncher.Plugins.Support where

import LambdaLauncher.Types

import Control.Monad (void)
import Data.List (isPrefixOf)
import System.Process (callProcess, spawnProcess)
import Data.Text (Text)

import qualified Data.Text as T


copyAction :: Priority -> Text -> Result
copyAction p s = Action s p $ void $ callProcess "wl-copy" [T.unpack s]

openUrlAction :: Text -> IO ()
openUrlAction s = void $ spawnProcess "xdg-open" [T.unpack s]

triggerOr :: IO [Result] -> Plugin -> Text -> Text -> IO [Result]
triggerOr o plug trig query =
  if trig `T.isPrefixOf` query
    then fmap (\r -> r {priority = 0}) <$> plug (T.drop (T.length trig) query)
    else o

trigger :: Plugin -> Text -> Plugin
trigger plug trig query = triggerOr (plug query) plug trig query

triggerStrict :: Plugin -> Text -> Plugin
triggerStrict = triggerOr (pure mempty)
