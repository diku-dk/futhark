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
    pkgs.git-annex
    pkgs.ghc
    pkgs.haskellPackages.weeder
    pkgs.hlint
    pkgs.pkgconfig
    pkgs.zlib
    pkgs.zlib.out
    pkgs.cabal2nix
    pkgs.ghcid
    pkgs.ormolu
    pkgs.niv
    pkgs.python3Packages.numpy
    pkgs.python3Packages.pyopencl
    pkgs.python3Packages.jsonschema
    pkgs.python3Packages.sphinx
    pkgs.python3Packages.sphinxcontrib-bibtex
    pkgs.imagemagick # needed for literate tests
  ]
  ++ pkgs.lib.optionals (pkgs.stdenv.isLinux)
    [ pkgs.opencl-headers
      pkgs.ocl-icd ]
  ;
}
