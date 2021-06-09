{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, lib, megaparsec, mtl, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck, text, vector, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.0.0.1";
  sha256 = "95ebad0fc7a459b756533471b3637ac4ece1f5de7e7239896c5481995f3ccb88";
  libraryHaskellDepends = [
    base binary bytestring bytestring-to-vector containers megaparsec
    mtl text vector vector-binary-instances
  ];
  testHaskellDepends = [
    base binary bytestring megaparsec QuickCheck tasty tasty-hunit
    tasty-quickcheck text vector
  ];
  description = "An implementation of the Futhark data format";
  license = lib.licenses.isc;
}
