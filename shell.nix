# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python312.withPackages (ps: with ps; [
    ps.mypy
    black
    cycler
    numpy
    pyopencl
    matplotlib
    jsonschema
    sphinx
    sphinxcontrib-bibtex
  ]);
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
      python
      xdot
      hlint
      pkg-config
      zlib
      zlib.out
      cabal2nix
      ghcid
      niv
      ispc
      imagemagick # needed for literate tests
      glpk
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
        rocmPackages.clr
      ]
  ;
}
