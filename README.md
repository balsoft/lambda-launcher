# λauncher [![Build Status](https://travis-ci.com/balsoft/lambda-launcher.svg?branch=master)](https://travis-ci.com/balsoft/lambda-launcher)

λauncher is a GTK launcher application built with Haskell and 
gtk-gi-declarative.

## Features

 -  Asynchronous plugin result loading
 -  No runtime settings (not a bug, but a feature)
 -  5M memory on coldstart, 20M memory when all plugins are loaded
 -  Very quick startup (compared to some other graphical launchers

## Configuration
All configuration at this moment is done by editing source files. This will be changed in the future to XMonad-style configuration.

### Plugins
To configure the plugins, edit `plugins :: [Plugin]` in `src/Main.hs`. You should be able to just edit the list of plugins to disable the unneeded ones. All of plugins are enabled by default.
#### Triggers
Apply `trigger` to a plugin to make it the only plugin shown when query starts with a trigger, and apply `triggerStrict` to show a plugin only when it's triggered.

### Window
Edit `configuration` top-level binding in `src/Main.hs`. The `Configuration` type constructor's field names should be self-explanatory.

## Building
### Nix
`nix build` should do everything for you.
### Cabal
`cabal new-run` works for me. If it doesn't, add an issue!

## Current list of plugins 
| Name          | Description                                     | Required dependencies | Optional dependencies |
|---------------|-------------------------------------------------|-----------------------|-----------------------|
| Command       | run shell commands, with suggestions            |                       |                       |
| Duckduckgo    | search the web with DDG instant answers         |                       |                       |
| Emacs         | open emacs in a directory from ~/projects       | emacs                 | nix-shell             |
| Files         | list and open files                             |                       |                       |
| Google        | search some text through google's web interface |                       |                       |
| Qalc          | use libqalculate to evaluate expressions        | qalc                  |                       |
| Stackoverflow | search for questions with similar titles        |                       |                       |
| Sway          | list and focus windows on sway                  | swaymsg               |                       |
| Wiki          | search english wikipedia                        |                       |                       |
| Wmctrl        | list and focus windows on X11                   | wmctrl                |                       |

If a plugin doesn't work, make sure you've installed the corresponding dependencies!

## Extending
 -  Create `src/Plugins/YourPlugin.hs` that exports 
    `yourPlugin :: String -> IO [Types.Result]`
 -  Add the corresponding import to `src/Plugins/Main.hs`
 -  Don't forget to add `yourPlugin` to `plugins` list
 -  Send a PR!

## Side note

I'm very new to Haskell, so any corrections to code
style/architecture/etc are welcome. Send issues.


## Code of conduct

All contributions are welcome! There are no restrictions.

## License

This work is in public domain. By sending Pull Requests you agree that
all of your work assosiated with this software is in public domain as
well.

This only applies to the code contained in this repository, to see
licenses of it's dependencies visit their respective pages.

## WARRANTY

THIS SOFTWARE COMES "AS IS", WITHOUT ANY WARRANTY (IMPLIED OR
OTHERWISE) TO THE EXTENT PERMITTED BY THE APPLICABLE LAWS. IT MAY
CAUSE DATA CORRUPTION OR LOSS. BY RUNNING THIS SOFTWARE YOU AGREE 
THAT IT'S AUTHOR(S) ARE NOT RESPONSIBLE FOR ANY DAMAGE CAUSED BY 
THIS SOFTWARE.

