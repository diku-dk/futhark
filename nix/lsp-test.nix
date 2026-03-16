{ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
, bytestring, co-log-core, conduit, conduit-parse, containers
, data-default, Diff, directory, exceptions, extra, filepath, Glob
, hspec, lens, lens-aeson, lib, lsp, lsp-types, mtl
, parser-combinators, process, some, text, time, transformers, unix
, unliftio
}:
mkDerivation {
  pname = "lsp-test";
  version = "0.18.0.0";
  sha256 = "03ae4cacdd60616e5eba2a02abb91e2192c134ff0cbeee889edc268ddc92d091";
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal async base bytestring co-log-core
    conduit conduit-parse containers data-default Diff directory
    exceptions extra filepath Glob lens lens-aeson lsp lsp-types mtl
    parser-combinators process some text time transformers unix
  ];
  testHaskellDepends = [
    aeson base co-log-core containers data-default directory extra
    filepath hspec lens lsp mtl parser-combinators process text
    unliftio
  ];
  testToolDepends = [ lsp ];
  benchmarkHaskellDepends = [ base extra lsp process ];
  doCheck = false;
  homepage = "https://github.com/haskell/lsp/blob/master/lsp-test/README.md";
  description = "Functional test framework for LSP servers";
  license = lib.licenses.bsd3;
}
