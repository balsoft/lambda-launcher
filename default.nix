{ pkgs ? (import (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    rev = "acbdaa569f4ee387386ebe1b9e60b9f95b4ab21b";
  }) { }), ... }@args:
let
  lambda-launcher-unwrapped =
  pkgs.haskellPackages.callPackage ./lambda-launcher.nix {};
  lambda-launcher =
  pkgs.callPackage ./wrapper.nix { inherit lambda-launcher-unwrapped; };
in if builtins.getEnv "IN_NIX_SHELL" != "" then
  lambda-launcher-unwrapped.env
else {
  inherit lambda-launcher lambda-launcher-unwrapped;
}
