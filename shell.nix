{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "futhark";
  buildInputs = [ zlib zlib.out pkgconfig haskell.compiler.ghc882 ];
}
