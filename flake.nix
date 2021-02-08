{
  description = "A launcher written in Haskell";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/dbcd55844106241cf2d6aa0bc6ccb1613d1a5ed4";

  outputs = { self, nixpkgs }: {
    packages = builtins.mapAttrs (_: pkgs: rec {
      lambda-launcher-unwrapped =
        pkgs.haskellPackages.callCabal2nix "lambda-launcher" self { };
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
        inputsFrom = [
          self.packages.${arch}.lambda-launcher-unwrapped.env
          self.packages.${arch}.lambda-launcher
        ];
      }) nixpkgs.legacyPackages;
  };
}
