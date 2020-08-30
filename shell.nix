let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  buildInputs = [
    pkgs.cabal-install
    pkgs.cacert
    pkgs.curl
    pkgs.file
    pkgs.git
    pkgs.haskell.compiler.ghc8101
    pkgs.hlint
    pkgs.ocl-icd
    pkgs.opencl-headers
    pkgs.pkgconfig
    pkgs.zlib
    pkgs.zlib.out
    pkgs.zlib.dev
    pkgs.cabal2nix
  ];
in
pkgs.stdenv.mkDerivation {
  name = "futhark";
  buildInputs = buildInputs;

  shellHook = ''export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH'';
}
