{
  pkgs ? import (import ./nix/sources.nix).nixpkgs { },
}:
let
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc98;
in
pkgs.mkShell {
  name = "futhark-style";
  packages = import ./nix/pkgs-style.nix { inherit pkgs haskell python; };
}
