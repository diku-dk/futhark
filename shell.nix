{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "futhark";
  buildInputs = [
    cabal-install
    cacert
    curl
    file
    git
    haskell.compiler.ghc8101
    hlint
    ocl-icd
    opencl-headers
    pkgconfig
    zlib
    zlib.out
  ];
}
