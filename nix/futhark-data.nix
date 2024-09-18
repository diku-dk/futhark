{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, half, lib, megaparsec, mtl, QuickCheck, scientific
, tasty, tasty-hunit, tasty-quickcheck, text, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.1.1.0";
  sha256 = "0ef011fb779f269208c0a6b57a62e1a5ec265bfd0cde820edf400cef57451804";
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
