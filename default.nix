{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

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
        executableHaskellDepends = [
          aeson async base bytestring data-default-class directory filepath
          gi-gdk gi-glib gi-gobject gi-gtk gi-gtk-declarative
          gi-gtk-declarative-app-simple haskell-gi haskell-gi-base process
          req text vector
        ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
  
  nix-bundle-src = "${pkgs.nix-bundle}/share/nix-bundle";
  nix-bundle = (import ("${nix-bundle-src}/appimage-top.nix") {}) // (import "${nix-bundle-src}/default.nix" {});

in

  if pkgs.lib.inNixShell then drv.env else (drv // {bundle = nix-bundle.nix-bootstrap { extraTargets = []; run = "/bin/lambda-launcher"; target = drv; };})
