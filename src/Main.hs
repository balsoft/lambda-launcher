{-# LANGUAGE OverloadedStrings #-}

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
  , trigger command "$ "
  , trigger kill "kill "
  , triggerStrict googletranslate "tr "
  , triggerStrict wiki "wiki "
  , triggerStrict duckduckgo "ddg "
  , triggerStrict stackoverflow "so "
  , triggerStrict pass "pass "
  , triggerStrict urbanDictionary "ub"
  , triggerStrict clipman "cp "
  , triggerStrict pulseaudio "pa "
  ]

conf :: Configuration
conf = Configuration
  { width = 500
  , maxHeight = 300
  , maxChars = 60
  , showBorder = False 
  , maxItemsPerPlugin = 4 }

main :: IO ()
main = runApp conf plugins
