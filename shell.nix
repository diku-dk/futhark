# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc98;
in
pkgs.stdenv.mkDerivation {
  name = "futhark";
  buildInputs =
    (import ./nix/pkgs-style.nix {pkgs=pkgs; haskell=haskell; python=python;}) ++
    (with pkgs;
    [
      cabal-install
      cacert
      curl
      file
      git
      haskell.ghc
      haskell.weeder
      haskell.haskell-language-server
      haskellPackages.graphmod
      haskellPackages.apply-refact
      xdot
      pkg-config
      zlib
      zlib.out
      cabal2nix
      ghcid
      niv
      ispc
      python.python
      python.mypy
      python.black
      python.cycler
      python.numpy
      python.pyopencl
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
      ])
  ;
}
