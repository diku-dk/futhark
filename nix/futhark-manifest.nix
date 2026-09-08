{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.10.0.0";
  sha256 = "1bf0e6b9ce759cbedb0260b9fa689c04e172bf54f3b5eedf6eeaae89ad80d9bd";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  doCheck = false;
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.licensesSpdx."ISC";
}
