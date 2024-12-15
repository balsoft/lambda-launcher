{-# LANGUAGE RecordWildCards #-}

module LambdaLauncher.Plugins.Pulseaudio where

import LambdaLauncher.Types
import Data.Text (pack)
import System.Process (callProcess, readProcess)

data Sink = Sink {sinkId :: Int, sinkName :: String}

parseSink :: String -> Sink
parseSink s = Sink (read (props !! 0)) (props !! 1)
  where
    props = words s

sinkToResult :: Sink -> LambdaLauncher.Types.Result
sinkToResult Sink {..} =
  Action (pack ("Set " <> sinkName <> " as default")) 2 $ callProcess "pactl" ["set-default-sink", show sinkId]

pulseaudio :: Plugin
pulseaudio s =
  map (sinkToResult . parseSink) . lines <$> readProcess "pactl" ["list", "sinks", "short"] ""

