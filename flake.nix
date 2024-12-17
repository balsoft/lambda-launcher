{
  description = "A launcher written in Haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        jailbreakUnbreak =
          pkg:
          pkgs.haskell.lib.doJailbreak (
            pkg.overrideAttrs (_: {
              meta = { };
            })
          );
        haskellPackages = pkgs.haskellPackages.extend (final: prev: {
          gi-gtk-declarative = jailbreakUnbreak pkgs.haskellPackages.gi-gtk-declarative;
          gi-gtk-declarative-app-simple = pkgs.haskell.lib.dontCheck (
            jailbreakUnbreak (
              pkgs.haskellPackages.gi-gtk-declarative-app-simple.override {
                inherit (final) gi-gtk-declarative;
              }
            )
          );

        });
      in
      {
        packages = rec {
          lambda-launcher-unwrapped = haskellPackages.callCabal2nix "lambda-launcher" self { };
          lambda-launcher = pkgs.callPackage ./wrapper.nix { inherit lambda-launcher-unwrapped; };
          default = lambda-launcher;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/lambda-launcher";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
          ];
          inputsFrom = [
            self.packages.${system}.lambda-launcher-unwrapped.env
            self.packages.${system}.lambda-launcher
          ];
        };
      }
    );
}
