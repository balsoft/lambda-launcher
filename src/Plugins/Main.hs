module Plugins.Main where

import Plugins.Command
import Plugins.Emacs
import Plugins.Files
import Plugins.Google
import Plugins.Qalc
import Plugins.Stackoverflow
import Plugins.Sway
import Plugins.Wiki
import Types

plugins :: [String -> IO [Result]]
plugins = [google, qalc, emacs, sway, files, command, wiki, stackoverflow]
