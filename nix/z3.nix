{ pkgs, mkDerivation, base, containers, fetchgit, gomp, hspec, lib
, QuickCheck, transformers, z3
}:
mkDerivation {
  pname = "z3";
  version = "408.2";
  src = fetchgit {
    url = "https://github.com/Munksgaard/haskell-z3";
    sha256 = "041qark149xfkg859rs0927hdryl56rhy7r6zryy6wk3ygfmb1l5";
    rev = "23f6eebe199c292c8d556c52a245e64d4b3caa92";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers transformers ];
  librarySystemDepends = [ pkgs.gomp pkgs.z3 ];
  testHaskellDepends = [ base hspec QuickCheck ];
  homepage = "https://github.com/IagoAbal/haskell-z3";
  description = "Bindings for the Z3 Theorem Prover";
  license = lib.licenses.bsd3;
}
