{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, filepath, hashable, hspec, hspec-discover, lens, lib, lsp-types
, mtl, prettyprinter, random, row-types, sorted-list, stm
, temporary, text, text-rope, transformers, unliftio-core
, unordered-containers, uuid
}:
mkDerivation {
  pname = "lsp";
  version = "2.1.0.0";
  sha256 = "c8a7a2b82d074641c77894639bdd5aacae5046610ee8d6b8a74b0cf71c4af30d";
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
