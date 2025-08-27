# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python311Packages;
  haskell = pkgs.haskell.packages.ghc96;
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
      haskell.ghc
      ormolu
      haskell.weeder
      haskell.haskell-language-server
      haskellPackages.graphmod
      haskellPackages.apply-refact
      hlint
      pkg-config
      zlib
      zlib.out
      cabal2nix
      ghcid
      niv
      python.python
      python.mypy
      python.black
      python.cycler
      python.numpy
      python.matplotlib
      python.jsonschema
      python.sphinx
      python.sphinxcontrib-bibtex
      imagemagick # needed for literate tests
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
        rocmPackages.clr
        python.pyopencl
        xdot
        ispc
      ]
  ;
}
