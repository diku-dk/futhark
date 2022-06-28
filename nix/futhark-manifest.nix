{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.1.0.0";
  sha256 = "f676f515ab0d9ab5805f04a6e05d20d5641ba93fd6c4fd233aa79a9fcd1a2ef8";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.licenses.isc;
}
