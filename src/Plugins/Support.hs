module Plugins.Support where

import Types

import System.Process (spawnProcess)

copyAction :: Priority -> String -> Result
copyAction p s =
  Action s p $ do
    _ <- spawnProcess "wl-copy" [s]
    return ()

openUrlAction :: String -> IO ()
openUrlAction s = do
  _ <- spawnProcess "xdg-open" [s]
  return ()
