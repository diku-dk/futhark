{ mkDerivation, alex, array, base, bytestring, containers
, exception-mtl, exception-transformers, filepath, happy
, haskell-src-meta, HUnit, lib, mainland-pretty, mtl, srcloc, syb
, template-haskell, test-framework, test-framework-hunit
}:
let
  lcq = builtins.fetchGit {
    url = "https://github.com/pema99/language-c-quote";
    ref = "ispc";
    rev = "f1cdbdca33a79d8e9301cad981fd35ef7129f2d6";
  };
in
mkDerivation {
  pname = "language-c-quote";
  version = "0.13";
  src = lcq;
  libraryHaskellDepends = [
    array base bytestring containers exception-mtl
    exception-transformers filepath haskell-src-meta mainland-pretty
    mtl srcloc syb template-haskell
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [
    base bytestring HUnit mainland-pretty srcloc test-framework
    test-framework-hunit
  ];
  homepage = "https://github.com/mainland/language-c-quote";
  description = "C/CUDA/OpenCL/Objective-C/ISPC quasiquoting library";
  license = lib.licenses.bsd3;
}
