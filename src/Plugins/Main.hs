module Plugins.Main where

import Plugins.Command
import Plugins.Duckduckgo
import Plugins.Emacs
import Plugins.Files
import Plugins.Qalc
import Plugins.Stackoverflow
import Plugins.Sway
import Plugins.Wiki
import Plugins.Wmctrl
import Types

import Plugins.Support (trigger, triggerStrict)

plugins :: [Plugin]
plugins =
  [ trigger qalc "qalc "
  , trigger emacs "em "
  , trigger sway "sw "
  , trigger wmctrl "win "
  , trigger files "file "
  , command
  , trigger wiki "wiki "
  , trigger duckduckgo "ddg "
  , trigger stackoverflow "so "
  ]
