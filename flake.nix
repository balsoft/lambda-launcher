{
  description = "A launcher written in Haskell";

  inputs.nixpkgs.url = github:balsoft/nixpkgs/fix-gi-gtk-declarative;

  outputs = { self, nixpkgs }: {
    packages = builtins.mapAttrs (_: pkgs: rec {
      lambda-launcher-unwrapped = pkgs.haskellPackages.callPackage ./lambda-launcher.nix { };
      lambda-launcher = pkgs.callPackage ./wrapper.nix { inherit lambda-launcher-unwrapped; };
    }) nixpkgs.legacyPackages;

    defaultPackage = builtins.mapAttrs (_: pkgs: pkgs.lambda-launcher) self.packages;

    defaultApp = builtins.mapAttrs (_: pkg: {
      type = "app";
      program = "${pkg}/bin/lambda-launcher";
    }) self.defaultPackage;
  };
}
