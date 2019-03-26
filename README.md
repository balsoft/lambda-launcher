# λauncher

λauncher is a GTK launcher application built with Haskell and 
gtk-gi-declarative.

## Features

 -  Asynchronous plugin result loading
 -  No runtime settings (not a bug, but a feature)
 -  4k memory on coldstart, 15k memory when all plugins are loaded
 -  Very quick startup (compared to some other graphical launchers

## Building
### Nix
`nix build` should do everything for you.

## Current list of plugins
 -  Command (running shell commands, command suggestion)
 -  Emacs (opening emacs in a directory from ~/projects)
 -  Files (listing and opening files)
 -  Google (search some text through google's web interface)
 -  Qalc (use libqalculate to evaluate expressions)
 -  Stackoverflow (search for questions with similar titles)
 -  Sway (list and focus windows)
 -  Wiki (search english wikipedia)

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

