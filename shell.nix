# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "futhark";
  buildInputs =
    with pkgs;
    [
      cabal-install
      cacert
      curl
      file
      git
      parallel
      haskell.compiler.ghc927
      haskell.packages.ghc927.ormolu
      haskell.packages.ghc927.weeder
      haskell.packages.ghc927.haskell-language-server
      haskellPackages.graphmod
      haskellPackages.apply-refact
      xdot
      hlint
      pkgconfig
      zlib
      zlib.out
      cabal2nix
      ghcid
      niv
      ispc
      python3Packages.mypy
      python3Packages.black
      python3Packages.numpy
      python3Packages.pyopencl
      python3Packages.matplotlib
      python3Packages.jsonschema
      python3Packages.sphinx
      python3Packages.sphinxcontrib-bibtex
      imagemagick # needed for literate tests
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
      ]
  ;
}
