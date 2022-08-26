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
      git-annex
      haskell.compiler.ghc924
      parallel
      haskell.packages.ghc924.weeder
      haskellPackages.graphmod
      haskellPackages.apply-refact
      xdot
      hlint
      pkgconfig
      zlib
      zlib.out
      cabal2nix
      ghcid
      haskell.packages.ghc924.ormolu_0_5_0_1
      niv
      ispc
      python3Packages.numpy
      python3Packages.pyopencl
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
