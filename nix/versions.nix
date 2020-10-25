{ mkDerivation, base, deepseq, hashable, megaparsec, microlens
, parser-combinators, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "versions";
  version = "4.0.1";
  sha256 = "0a153d0ba4ad6ecb3bab917ab4138b3ac0e80d9d703bbd1ee7046b3570b70be9";
  libraryHaskellDepends = [
    base deepseq hashable megaparsec parser-combinators text
  ];
  testHaskellDepends = [
    base megaparsec microlens QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  homepage = "https://github.com/fosskers/versions";
  description = "Types and parsers for software version numbers";
  license = stdenv.lib.licenses.bsd3;
}
