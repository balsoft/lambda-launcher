module LambdaLauncher.Types where

import GHC.Int (Int32)
import Data.Text (Text)
import GI.Gtk.Objects (Image)

instance Eq Image 

type Priority = Integer

data Result
  = Action
    { shownText :: Text
    , image :: Maybe Image
    , priority :: Priority
    , action :: IO ()
    }

type Plugin = Text -> IO [Result]

-- Configuration!
data Configuration = Configuration
  { width :: Int32
  , maxHeight :: Int32
  , maxChars :: Int
  , showBorder :: Bool }
