{ nixpkgs ? import <nixpkgs>, compiler ? "default", doBenchmark ? false }:

let

  pkgs = (nixpkgs {
    overlays = [
      (self: super: {
        libjpeg_turbo = super.libjpeg_turbo.overrideAttrs (old: {doCheck = false;});
      })
    ];
  }).pkgsMusl;

  f = { mkDerivation, aeson, async, base, bytestring
      , data-default-class, directory, filepath, gi-gdk, gi-glib
      , gi-gobject, gi-gtk, gi-gtk-declarative
      , gi-gtk-declarative-app-simple, haskell-gi, haskell-gi-base
      , process, req, stdenv, text, vector
      }:
      mkDerivation {
        pname = "lambda-launcher";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        configureFlags = [
          "--ghc-option=-optl=-static"
        ];
        
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        executableHaskellDepends = [
          aeson async base bytestring data-default-class directory filepath
          gi-gdk gi-glib gi-gobject gi-gtk gi-gtk-declarative
          gi-gtk-declarative-app-simple haskell-gi haskell-gi-base process
          req text vector
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  normalHaskellPackages = 
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  
  haskellPackages = with pkgs.haskell.lib; normalHaskellPackages.override {
    overrides = self: super: {
      # Dependencies we need to patch
      aeson = super.aeson.overrideAttrs (oldAttrs: {dontCheck = true;});
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
  
  nix-bundle-src = builtins.fetchGit {
    url = "https://github.com/matthewbauer/nix-bundle";
    rev = "e8d57a08bc1912ada13af44f997e29e88e2fbb66";
  };
  nix-bundle = (import ("${nix-bundle-src}/appimage-top.nix") {}) // (import "${nix-bundle-src}/default.nix" {});
in 
if pkgs.lib.inNixShell
  then drv.env 
  else 
    (drv // {
      bundle = nix-bundle.nix-bootstrap { 
        run = "/bin/lambda-launcher"; 
        target = drv; 
        nixUserChrootFlags = "-p DISPLAY -p GTK_THEME -m /etc:${pkgs.iana-etc}/etc"; 
      };
    })
