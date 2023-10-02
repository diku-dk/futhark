{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, filepath, hashable, hspec, hspec-discover, lens, lens-aeson, lib
, lsp-types, mtl, prettyprinter, random, row-types, sorted-list
, stm, temporary, text, text-rope, transformers, unliftio-core
, unordered-containers, uuid
}:
mkDerivation {
  pname = "lsp";
  version = "2.2.0.0";
  sha256 = "592a2d5df3bfc100320ee98d978694dd7d8e5568364daab42f767fb94f9f1d5b";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring co-log-core containers
    data-default directory exceptions filepath hashable lens lens-aeson
    lsp-types mtl prettyprinter random row-types sorted-list stm
    temporary text text-rope transformers unliftio-core
    unordered-containers uuid
  ];
  testHaskellDepends = [
    base containers hspec row-types sorted-list text text-rope
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
