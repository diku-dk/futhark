{ mkDerivation, array, base, containers, deepseq, fetchgit, gasp
, glpk, lib, mtl
}:
mkDerivation {
  pname = "glpk-hs";
  version = "0.8";
  src = fetchgit {
    url = "https://github.com/ludat/glpk-hs.git";
    sha256 = "0nly5nifdb93f739vr3jzgi16fccqw5l0aabf5lglsdkdad713q1";
    rev = "efcb8354daa1205de2b862898353da2e4beb76b2";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base containers deepseq gasp mtl ];
  librarySystemDepends = [ glpk ];
  executableHaskellDepends = [
    array base containers deepseq gasp mtl
  ];
  description = "Comprehensive GLPK linear programming bindings";
  license = lib.licenses.bsd3;
  mainProgram = "glpk-hs-example";
}
