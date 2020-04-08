# This default.nix builds a statically linked futhark binary.
#
# It currently does not build documentation and is likely to only work
# on Linux.
#
# Just run 'nix-build' and fish the binary out of 'result/bin/futhark'.

{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
let
  pkgs = nixpkgs;

  futhark =
    pkgs.haskell.packages.${compiler}.callCabal2nix "futhark"
    ( pkgs.lib.cleanSource ./. ) { };
in
  pkgs.haskell.lib.overrideCabal
    futhark
    ( oldDrv: {
      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      enableLibraryProfiling = false;
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--ghc-option=-split-sections"
        "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.glibc.static}/lib"
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ];
      }
    )
