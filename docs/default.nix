with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "futhark-docs";
    buildInputs = [ pythonPackages.sphinx pythonPackages.sphinxcontrib-bibtex ];
}
