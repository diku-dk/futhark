{ mkDerivation, aeson, base, binary, containers, data-default
, deepseq, Diff, directory, dlist, exceptions, file-embed, filepath
, generic-arbitrary, hashable, hspec, hspec-discover, hspec-golden
, indexed-traversable, indexed-traversable-instances, lens, lib
, mod, mtl, network-uri, prettyprinter, QuickCheck
, quickcheck-instances, regex, row-types, safe, some
, template-haskell, text, witherable
}:
mkDerivation {
  pname = "lsp-types";
  version = "2.4.0.0";
  sha256 = "2bd17496aa8cc9024d27525ea0d1c797f89554a6248608fec9cfc87e5b403f2e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq Diff dlist
    exceptions file-embed filepath generic-arbitrary hashable
    indexed-traversable indexed-traversable-instances lens mod mtl
    network-uri prettyprinter QuickCheck quickcheck-instances row-types
    safe some template-haskell text
  ];
  executableHaskellDepends = [
    base containers directory filepath mtl prettyprinter regex text
    witherable
  ];
  testHaskellDepends = [
    aeson base filepath hspec hspec-golden lens network-uri
    prettyprinter QuickCheck quickcheck-instances text
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
  mainProgram = "generator";
}
