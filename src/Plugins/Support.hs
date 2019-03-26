module Plugins.Support where

import Types

import Control.Monad (void)
import Data.List (isPrefixOf)
import System.Process (callProcess, spawnProcess)

copyAction :: Priority -> String -> Result
copyAction p s = Action s p $ void $ callProcess "wl-copy" [s]

openUrlAction :: String -> IO ()
openUrlAction s = void $ spawnProcess "xdg-open" [s]

trigger :: Plugin -> String -> Plugin
trigger plug trig =
  (\query ->
     if trig `isPrefixOf` query
       then plug $ drop (length trig) query
       else plug query)

triggerStrict :: Plugin -> String -> Plugin
triggerStrict plug trig =
  (\query ->
     if trig `isPrefixOf` query
       then plug $ drop (length trig) query
       else pure mempty)
