{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, filepath, hashable, hspec, hspec-discover, lens, lib, lsp-types
, mtl, prettyprinter, random, row-types, sorted-list, stm
, temporary, text, text-rope, transformers, unliftio-core
, unordered-containers, uuid
}:
mkDerivation {
  pname = "lsp";
  version = "2.0.0.0";
  sha256 = "af73311e471a1d929858e6ba05c11cc950da4bb565286e9dee3b35a0095e11e6";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring co-log-core containers
    data-default directory exceptions filepath hashable lens lsp-types
    mtl prettyprinter random row-types sorted-list stm temporary text
    text-rope transformers unliftio-core unordered-containers uuid
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
