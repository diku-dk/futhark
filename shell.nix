# See header comment in default.nix for how to update sources.nix.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  pps = ps: with ps; [
    (
      buildPythonPackage rec {
        pname = "PuLP";
        version = "2.7.0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-5z7msy1jnJuM9LSt7TNLoVi+X4MTVE4Fb3lqzgoQrmM=";
        };
        doCheck = false;
      }
    )
  ];
  python = pkgs.python3.withPackages pps;
in
pkgs.stdenv.mkDerivation {
  name = "futhark";
  buildInputs =
    with pkgs;
    [
      cabal-install
      cacert
      curl
      file
      git
      parallel
      haskell.compiler.ghc94
      ormolu
      haskell.packages.ghc94.weeder
      #haskell.packages.ghc94.haskell-language-server
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
      python3Packages.mypy
      python3Packages.black
      python3Packages.numpy
      python3Packages.pyopencl
      python3Packages.matplotlib
      python3Packages.jsonschema
      python3Packages.sphinx
      python3Packages.sphinxcontrib-bibtex
      imagemagick # needed for literate tests
      # remove (needed for PuLP)
      python
      cbc
      glpk
    ]
    ++ lib.optionals (stdenv.isLinux)
      [ opencl-headers
        ocl-icd
        oclgrind
        rocmPackages.clr
      ]
  ;
}
