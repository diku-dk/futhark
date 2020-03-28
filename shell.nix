{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "futhark";
  buildInputs = [ zlib zlib.out pkgconfig haskell.compiler.ghc8101 cabal-install ];
}
