with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "futhark-docs";
    buildInputs = [ python37 python37Packages.sphinx ];
}
