{-# LANGUAGE RecordWildCards #-}

module LambdaLauncher.Plugins.Pulseaudio where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map (Map, fromList, (!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import LambdaLauncher.Types
import System.Process (callProcess, readProcess)
import Text.Fuzzy as F
import Text.Parsec

data Sink = Sink {sinkId :: Int, sinkName :: String} deriving (Show)

sinksList :: Parsec String st [Sink]
sinksList = many sink <* eof

sink :: Parsec String st Sink
sink = do
  string "Sink #"
  sinkId <- read <$> many1 digit <* endOfLine
  props <- fromList <$> many1 property
  let sinkName = fromMaybe (fromMaybe (show sinkId) (props !? "Name")) (props !? "Description")
  optional endOfLine
  pure Sink {..}

property :: Parsec String st (String, String)
property = do
  tab
  name <- many1 (letter <|> char ' ')
  char ':'
  value <- multiLineProp
  pure (name, value)
  where
    singleLineProp = char ' ' *> many1 (noneOf ['\n']) <* endOfLine
    tabOrSpaces = tab <|> (many1 (char ' ') $> '\t')
    multiLineProp =
      ((endOfLine $> "") <|> singleLineProp)
        <> (mconcat <$> many (try (tab *> tabOrSpaces *> (many1 (noneOf ['\n']) <> string "\n"))))

sinkToResult :: Sink -> LambdaLauncher.Types.Result
sinkToResult Sink {..} =
  Action (pack ("Set " <> sinkName <> " as default")) 2 $ callProcess "pactl" ["set-default-sink", show sinkId]

filterSinks :: String -> [Sink] -> [Sink]
filterSinks f s
  | f == "" = s
  | otherwise = F.original <$> F.filter f s "" "" sinkName False

pulseaudio :: Plugin
pulseaudio s = do
  (map sinkToResult . filterSinks (unpack s) . fromRight []) . parse sinksList "stdin"
    <$> readProcess "pactl" ["list", "sinks"] ""
