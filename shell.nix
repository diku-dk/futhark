# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python313.withPackages (ps: with ps; [
    black
    cycler
    numpy
    pyopencl
    matplotlib
    jsonschema
    sphinx
    sphinxcontrib-bibtex
  ]);
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
      python
      xdot
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
      ])
  ;
}
