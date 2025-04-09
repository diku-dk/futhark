# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  python = pkgs.python312Packages;
  haskell = pkgs.haskell.packages.ghc96;
  PWD = builtins.getEnv "PWD";
in
pkgs.stdenv.mkDerivation {
  name = "futhark";

  EM_CACHE = "${PWD}/em_cache";

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
      xdot
      hlint
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
      # The following are for WebGPU.
    ++ [
      emscripten
      python3Packages.aiohttp
      python3Packages.selenium
      chromium
      chromedriver
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
        rocmPackages.clr
      ]
  ;
}
