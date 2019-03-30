module LambdaLauncher.Plugins.Qalc where

import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import LambdaLauncher.Plugins.Support (copyAction)
import System.Process (readProcess)
import LambdaLauncher.Types
import Data.Text (Text)

import qualified Data.Text as T


qalc :: Plugin
qalc s = do
  str <- T.pack <$> readProcess "qalc" [] (T.unpack s)
  let res = last . T.splitOn "="
        <$> listToMaybe (take 1 $ reverse $ take 3 $ T.lines str)
  return $
    concat $
    filter (\a -> not $ "error" `T.isInfixOf` shownText a) . pure . copyAction 1 <$> (res :: Maybe Text)
