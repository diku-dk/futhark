{ mkDerivation, base, deepseq, hashable, lib, megaparsec, microlens
, parser-combinators, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "versions";
  version = "6.0.0";
  sha256 = "d7df2b918522f31cc2d555ac626fa200b57e98fa2f70976c3541ed68a8d268b1";
  libraryHaskellDepends = [
    base deepseq hashable megaparsec parser-combinators text
  ];
  testHaskellDepends = [
    base megaparsec microlens tasty tasty-hunit text
  ];
  homepage = "https://github.com/fosskers/versions";
  description = "Types and parsers for software version numbers";
  license = lib.licenses.bsd3;
}
