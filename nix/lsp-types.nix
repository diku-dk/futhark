{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, directory, dlist, exceptions, file-embed, filepath
, hashable, hspec, hspec-discover, indexed-traversable
, indexed-traversable-instances, lens, lens-aeson, lib, mod, mtl
, network-uri, prettyprinter, QuickCheck, quickcheck-instances
, regex, row-types, safe, some, template-haskell, text
, unordered-containers
}:
mkDerivation {
  pname = "lsp-types";
  version = "2.0.2.0";
  sha256 = "4686b80ad6b98d7ca773bf5a25a0797093c96b236da8089690ec3ab4462f0969";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions file-embed filepath hashable indexed-traversable
    indexed-traversable-instances lens lens-aeson mod mtl network-uri
    prettyprinter row-types safe some template-haskell text
    unordered-containers
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
