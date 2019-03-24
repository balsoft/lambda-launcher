module Plugins.Support where

import Types

import System.Process (callProcess)

copyAction :: String -> Result
copyAction s = Action s $ callProcess "wl-copy" [s]

noAction :: String -> Result
noAction s = Action s $ return ()

openUrlAction :: String -> Result
openUrlAction s = Action s $ callProcess "xdg-open" [s]
