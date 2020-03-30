with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "futhark-docs";
    buildInputs = [ python37Packages.sphinx ];
}
