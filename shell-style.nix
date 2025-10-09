let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc98;
in
pkgs.stdenv.mkDerivation {
  name = "futhark-style";
  buildInputs =
    (import ./nix/pkgs-style.nix {pkgs=pkgs; haskell=haskell; python=python;});
}
