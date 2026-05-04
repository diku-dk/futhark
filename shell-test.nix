{
  pkgs ? import (import ./nix/sources.nix).nixpkgs { },
}:
let
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc98;
in
pkgs.mkShell {
  name = "futhark-test";
  packages = import ./nix/pkgs-test.nix { inherit pkgs python; };
}
