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
      ghc
      parallel
      haskellPackages.weeder
      haskellPackages.graphmod
      haskellPackages.apply-refact
      xdot
      hlint
      pkgconfig
      zlib
      zlib.out
      cabal2nix
      ghcid
      haskell.packages.ghc923.ormolu_0_5_0_0
      niv
      ispc
      python3Packages.numpy
      python3Packages.pyopencl
      python3Packages.jsonschema
      python3Packages.sphinx
      python3Packages.sphinxcontrib-bibtex
      imagemagick # needed for literate tests
      z3
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
      ]
  ;
}
