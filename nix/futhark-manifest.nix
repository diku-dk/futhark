{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.0.0.0";
  sha256 = "356678ab2a4f111fd0fcbc1bfe2f50691f8167d5371bbf66b17f4997f57b91c3";
  revision = "1";
  editedCabalFile = "09azyha9hfbrzixvq70ps70qz4xfavf9i6zkrqgl3m424h19i0yk";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.licenses.isc;
}
