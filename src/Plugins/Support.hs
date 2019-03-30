module Plugins.Support where

import Types

import Control.Monad (void)
import Data.List (isPrefixOf)
import System.Process (callProcess, spawnProcess)

copyAction :: Priority -> String -> Result
copyAction p s = Action s p $ void $ callProcess "wl-copy" [s]

openUrlAction :: String -> IO ()
openUrlAction s = void $ spawnProcess "xdg-open" [s]

triggerOr o plug trig query =
  if trig `isPrefixOf` query
    then fmap (\r -> r {priority = 0}) <$> plug (drop (length trig) query)
    else o

trigger :: Plugin -> String -> Plugin
trigger plug trig query = triggerOr (plug query) plug trig query

triggerStrict :: Plugin -> String -> Plugin
triggerStrict = triggerOr (pure mempty)
