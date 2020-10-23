{ mkDerivation, aeson, alex, ansi-terminal, array, base, binary
, blaze-html, bytestring, cmark-gfm, containers, directory
, directory-tree, dlist, file-embed, filepath, free, gitrev, happy
, haskeline, language-c-quote, megaparsec, mtl
, neat-interpolation, parallel, parser-combinators, pcg-random, prettyprinter_1_7_0
, process, process-extras, QuickCheck, regex-tdfa, sexp-grammar_2_2_1
, srcloc, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, temporary, terminal-size, text, time
, transformers, unordered-containers, utf8-string, vector
, vector-binary-instances, versions, zip-archive, zlib
}:
mkDerivation {
  pname = "futhark";
  version = "0.17.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal array base binary blaze-html bytestring
    cmark-gfm containers directory directory-tree dlist file-embed
    filepath free gitrev haskeline language-c-quote
    megaparsec mtl neat-interpolation parallel pcg-random prettyprinter_1_7_0 process
    process-extras regex-tdfa sexp-grammar_2_2_1 srcloc template-haskell
    temporary terminal-size text time transformers unordered-containers
    utf8-string vector vector-binary-instances versions zip-archive
    zlib
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base containers megaparsec mtl parser-combinators QuickCheck
    sexp-grammar_2_2_1 tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://futhark-lang.org";
  description = "An optimising compiler for a functional, array-oriented language";
  license = stdenv.lib.licenses.isc;
}
