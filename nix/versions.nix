{ mkDerivation, base, deepseq, hashable, lib, megaparsec, microlens
, parser-combinators, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "versions";
  version = "5.0.0";
  sha256 = "f92fed241d1c30ad53d4257a77b7543a07a48667be61d70e370c2bdd7694dde5";
  libraryHaskellDepends = [
    base deepseq hashable megaparsec parser-combinators text
  ];
  testHaskellDepends = [
    base megaparsec microlens QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  homepage = "https://github.com/fosskers/versions";
  description = "Types and parsers for software version numbers";
  license = lib.licenses.bsd3;
}
