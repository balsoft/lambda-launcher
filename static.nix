let
  nixpkgs = import <nixpkgs> {};
  
  lambda-launcher = import ./default.nix {inherit nixpkgs;};
  
  lambda-launcher-with-desktop = lambda-launcher.overrideAttrs (old:
    {
      desktopItem = nixpkgs.makeDesktopItem {
        name = "lambda-launcher";
        exec = "/bin/lambda-launcher";
        desktopName = "lambda-launcher";
        terminal = "false";
        categories = "Utility;";
        icon = "lambda-launcher";
      };
      postInstall = ''
        mkdir -p $out/share/icons/hicolor/256x256/apps
        touch $out/share/icons/hicolor/256x256/apps/lambda-launcher.png
        mkdir -p $out/share/applications
        substitute $desktopItem/share/applications/lambda-launcher.desktop $out/share/applications/lambda-launcher.desktop \
        --subst-var out
      '';
    }
  );

  nix-bundle-src = builtins.fetchGit {
    url = "https://github.com/matthewbauer/nix-bundle";
    rev = "e8d57a08bc1912ada13af44f997e29e88e2fbb66";
  };
  nix-bundle = (import ("${nix-bundle-src}/appimage-top.nix") {}) // (import "${nix-bundle-src}/default.nix" {});
in
(lambda-launcher // {
  bundle = nix-bundle.nix-bootstrap { 
    run = "/bin/lambda-launcher"; 
    target = lambda-launcher; 
    nixUserChrootFlags = "-p DISPLAY -p GTK_THEME"; 
  };
  appimage = nix-bundle.appimage (
    (nix-bundle.appdir {
      name = "lambda-launcher";
      target = lambda-launcher-with-desktop;
    }).overrideAttrs (old:
      {
        buildCommand = ''
        '' + old.buildCommand;
      }
    )
  );
})
