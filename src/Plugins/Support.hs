module Plugins.Support where

import Types

import Data.List (isPrefixOf)
import System.Process (callProcess, spawnProcess)

copyAction :: Priority -> String -> Result
copyAction p s =
  Action s p $ do
    _ <- callProcess "wl-copy" [s]
    return ()

openUrlAction :: String -> IO ()
openUrlAction s = do
  _ <- spawnProcess "xdg-open" [s]
  return ()

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
