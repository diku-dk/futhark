{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, half, lib, megaparsec, mtl, QuickCheck, scientific
, tasty, tasty-hunit, tasty-quickcheck, text, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.0.3.0";
  sha256 = "8a94de60fabb80b8dd4addbb3a5e4ecf5603a38996a3936baca5b95e9f8553d7";
  libraryHaskellDepends = [
    base binary bytestring bytestring-to-vector containers half
    megaparsec mtl scientific text vector vector-binary-instances
  ];
  testHaskellDepends = [
    base binary bytestring megaparsec QuickCheck tasty tasty-hunit
    tasty-quickcheck text vector
  ];
  description = "An implementation of the Futhark data format";
  license = lib.licenses.isc;
}
