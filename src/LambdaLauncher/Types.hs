module LambdaLauncher.Types where

import GHC.Int (Int32)

type Priority = Integer

data Result = Action
  { shownText :: String
  , priority :: Priority
  , action :: IO ()
  }

type Plugin = String -> IO [Result]


-- Configuration!
data Configuration = Configuration
  { width :: Int32
  , maxHeight :: Int32
  , maxChars :: Int
  }
