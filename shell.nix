{
  pkgs ? import (import ./nix/sources.nix).nixpkgs { },
}:
let
  python = pkgs.python313Packages;
  haskell = pkgs.haskell.packages.ghc910;
in
pkgs.mkShell {
  name = "futhark";
  packages =
    (import ./nix/pkgs-style.nix { inherit pkgs haskell python; })
    ++ (
      with pkgs;
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
        #      haskellPackages.apply-refact
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
      ++ lib.optionals (stdenv.isLinux) [
        opencl-headers
        ocl-icd
        (pkgs.callPackage ./nix/oclgrind.nix { })
        rocmPackages.clr
      ]
    );

  # Locale setup so that 'nix-shell --pure' will not run in an ASCII
  # environment.
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG = "C.UTF-8";
  LC_ALL = "C.UTF-8";
}
