module Main where
import LambdaLauncher.Main
import LambdaLauncher.Plugins
import LambdaLauncher.Types


import LambdaLauncher.Plugins.Support (trigger, triggerStrict)

plugins :: [Plugin]
plugins =
  [ trigger qalc "q "
  , trigger emacs "em "
  , triggerStrict google "g "
  , trigger sway "sw "
  , trigger wmctrl "win "
  , trigger files "file "
  , command
  , triggerStrict wiki "wiki "
  , triggerStrict duckduckgo "ddg "
  , triggerStrict stackoverflow "so "
  ]

conf :: Configuration
conf = Configuration
  { width = 500
  , maxHeight = 300
  , maxChars = 60 }

main :: IO ()
main = runApp conf plugins

