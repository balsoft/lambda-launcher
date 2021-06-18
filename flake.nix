{
  description = "A launcher written in Haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages = builtins.mapAttrs (_: pkgs:
      let
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      in rec {
        lambda-launcher-unwrapped =
          pkgs.haskellPackages.callCabal2nix "lambda-launcher" self rec {
            gi-gtk-declarative =
              jailbreakUnbreak pkgs.haskellPackages.gi-gtk-declarative;
            gi-gtk-declarative-app-simple = pkgs.haskell.lib.dontCheck
              (jailbreakUnbreak
                (pkgs.haskellPackages.gi-gtk-declarative-app-simple.override {
                  inherit gi-gtk-declarative;
                }));
          };
        lambda-launcher =
          pkgs.callPackage ./wrapper.nix { inherit lambda-launcher-unwrapped; };
      }) nixpkgs.legacyPackages;

    defaultPackage =
      builtins.mapAttrs (_: pkgs: pkgs.lambda-launcher) self.packages;

    defaultApp = builtins.mapAttrs (_: pkg: {
      type = "app";
      program = "${pkg}/bin/lambda-launcher";
    }) self.defaultPackage;

    devShell = builtins.mapAttrs (arch: pkgs:
      pkgs.mkShell {
        buildInputs = [ pkgs.haskell-language-server pkgs.cabal-install ];
        inputsFrom = [
          self.packages.${arch}.lambda-launcher-unwrapped.env
          self.packages.${arch}.lambda-launcher
        ];
      }) nixpkgs.legacyPackages;
  };
}
