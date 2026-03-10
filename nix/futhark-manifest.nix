{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.7.0.0";
  sha256 = "ec96330ba12b2dab9b3fc68c6543dcbd73f52c9b2616f177440f21c17ec544d0";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.licenses.isc;
}
