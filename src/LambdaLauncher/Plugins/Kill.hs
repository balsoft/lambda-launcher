{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaLauncher.Plugins.Kill
  ( kill
  ) where

import Data.List (isInfixOf)
import Data.Text (pack, unpack)
import LambdaLauncher.Types
import System.Process (callProcess, readProcess)

data Process = Process
  { pid :: Int
  , tty :: String
  , stat :: String
  , cpuTime :: String
  , command :: String
  } deriving (Eq, Show)

instance Read Process where
  readsPrec _ input =
    let w = words input
     in pure
        ( Process
            (read $ w !! 0)
            (w !! 1)
            (w !! 2)
            (w !! 3)
            (unwords $ drop 4 w)
        , "")

listProcesses :: IO [Process]
listProcesses = (fmap read <$> drop 1) . lines <$> readProcess "ps" ["-x"] ""

killProcess :: Int -> IO ()
killProcess pid = callProcess "kill" [show pid]

kill :: Plugin
kill s =
  fmap
    (\(Process {..}) -> Action (pack ("Kill " ++ command)) 3 $ killProcess pid) .
  filter (\(Process {..}) -> (unpack s) `isInfixOf` command) <$>
  listProcesses
