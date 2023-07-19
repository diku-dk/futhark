{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, directory, dlist, exceptions, file-embed, filepath
, hashable, hspec, hspec-discover, lens, lib, mod, mtl, network-uri
, prettyprinter, QuickCheck, quickcheck-instances, regex, row-types
, safe, some, template-haskell, text, unordered-containers
}:
mkDerivation {
  pname = "lsp-types";
  version = "2.0.1.0";
  sha256 = "57406c159d14aa30b3d8e5f48ca713186b340f668c93e940e884387fe561ffe0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions file-embed filepath hashable lens mod mtl network-uri
    row-types safe some template-haskell text unordered-containers
  ];
  executableHaskellDepends = [
    base containers directory filepath mtl prettyprinter regex text
  ];
  testHaskellDepends = [
    aeson base filepath hspec lens network-uri QuickCheck
    quickcheck-instances row-types text
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
  mainProgram = "generator";
}
