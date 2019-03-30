module LambdaLauncher.Plugins.Qalc where

import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import LambdaLauncher.Plugins.Support (copyAction)
import System.Process (readProcess)
import LambdaLauncher.Types

qalc :: Plugin
qalc s = do
  str <- readProcess "qalc" [] s
  let res =
        Prelude.head . reverse . map Text.unpack . Text.splitOn "=" . Text.pack <$>
        (listToMaybe $ take 1 $ reverse $ take 3 $ lines str)
  return $
    concat $
    filter (\a -> not $ "error" `isInfixOf` shownText a) . pure . copyAction 1 <$>
    (res :: Maybe String)
