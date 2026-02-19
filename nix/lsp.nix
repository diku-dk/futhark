{ mkDerivation, aeson, async, attoparsec, base, bytestring
, co-log-core, containers, data-default, directory, exceptions
, extra, filepath, hashable, hspec, hspec-discover, lens
, lens-aeson, lib, lsp-types, mtl, prettyprinter, sorted-list, stm
, text, text-rope, transformers, unliftio, unliftio-core
, unordered-containers, websockets
}:
mkDerivation {
  pname = "lsp";
  version = "2.8.0.0";
  sha256 = "d740f7aa36b95738da98f26e899d4ad491b94cfa977dbcc7b4b1097610ecb420";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring co-log-core containers
    data-default directory exceptions extra filepath hashable lens
    lens-aeson lsp-types mtl prettyprinter sorted-list stm text
    text-rope transformers unliftio unliftio-core unordered-containers
    websockets
  ];
  testHaskellDepends = [
    base containers hspec sorted-list text text-rope
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
