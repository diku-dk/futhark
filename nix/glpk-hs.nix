{ mkDerivation, array, base, containers, deepseq, fetchgit, gasp
, glpk, lib, mtl
}:
mkDerivation {
  pname = "glpk-hs";
  version = "0.8";
  src = fetchgit {
    url = "https://github.com/jyp/glpk-hs.git";
    sha256 = "sha256-AY9wmmqzafpocUspQAvHjDkT4vty5J3GcSOt5qItnlo=";
    rev = "1f276aa19861203ea8367dc27a6ad4c8a31c9062";
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
