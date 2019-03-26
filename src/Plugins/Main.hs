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

plugins :: [Plugin]
plugins =
  [qalc, emacs, sway, wmctrl, files, command, wiki, duckduckgo, stackoverflow]
