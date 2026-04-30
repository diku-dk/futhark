{
  pkgs ? import (import ../nix/sources.nix).nixpkgs { },
}:
pkgs.stdenv.mkDerivation {
  name = "futhark-docs";
  buildInputs = [ pkgs.python37Packages.sphinx ];
}
