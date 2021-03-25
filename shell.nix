let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "futhark";
  buildInputs = [
    pkgs.cabal-install
    pkgs.cacert
    pkgs.curl
    pkgs.file
    pkgs.git
    pkgs.haskell.compiler.ghc8104
    pkgs.haskellPackages.weeder
    pkgs.hlint
    pkgs.ocl-icd
    pkgs.opencl-headers
    pkgs.pkgconfig
    pkgs.zlib
    pkgs.zlib.out
    pkgs.cabal2nix
    pkgs.ghcid
    pkgs.ormolu
    pkgs.niv
    pkgs.imagemagick # needed for literate tests
  ];
}
