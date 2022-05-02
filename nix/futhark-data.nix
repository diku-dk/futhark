{ mkDerivation, base, binary, bytestring, bytestring-to-vector
, containers, half, lib, megaparsec, mtl, QuickCheck, scientific
, tasty, tasty-hunit, tasty-quickcheck, text, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "futhark-data";
  version = "1.1.0.0";
  sha256 = "a71cf118f67be4afdf252dc59ee78e24e9805c704f4a100dc6d27440d98dc44b";
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
