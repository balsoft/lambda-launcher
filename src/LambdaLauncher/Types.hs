module LambdaLauncher.Types where

import GHC.Int (Int32)
import Data.Text (Text)

type Priority = Integer

data Result = Action
  { shownText :: Text
  , priority :: Priority
  , action :: IO ()
  }

type Plugin = Text -> IO [Result]

-- Configuration!
data Configuration = Configuration
  { width :: Int32
  , maxHeight :: Int32
  , maxChars :: Int
  }
