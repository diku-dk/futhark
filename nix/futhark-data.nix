{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, half, lib, megaparsec, mtl, QuickCheck, tasty
, tasty-hunit, tasty-quickcheck, text, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.0.1.1";
  sha256 = "bc60c4e080f4106afb250f2905e3ae541851aaaac53ec879d9067f907586a3e5";
  libraryHaskellDepends = [
    base binary bytestring bytestring-to-vector containers half
    megaparsec mtl text vector vector-binary-instances
  ];
  testHaskellDepends = [
    base binary bytestring megaparsec QuickCheck tasty tasty-hunit
    tasty-quickcheck text vector
  ];
  description = "An implementation of the Futhark data format";
  license = lib.licenses.isc;
}
