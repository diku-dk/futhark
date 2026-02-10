let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc98;
in
pkgs.stdenv.mkDerivation {
  name = "futhark-test";
  buildInputs =
    (import ./nix/pkgs-test.nix {pkgs=pkgs; python=python;});
}
