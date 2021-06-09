{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, lib, megaparsec, mtl, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck, text, vector, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.0.0.0";
  sha256 = "c42b58809777358ef7e253e79ec691311daedf7ccd3f190056152602d353e615";
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
