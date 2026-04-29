{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.8.0.0";
  sha256 = "34d02dd39fcc6f8b7d43d24c8455fbc4ed2d88e1103cd190b3c5f60467ce02eb";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.licenses.isc;
}
