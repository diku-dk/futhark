{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, half, lib, megaparsec, mtl, QuickCheck, scientific
, tasty, tasty-hunit, tasty-quickcheck, text, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.0.2.0";
  sha256 = "ada137d505101b00033aa5a86ff3b39d8428513c9f33a17995ecd8d1f6cfaafc";
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
