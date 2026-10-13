{ mkDerivation, aeson, base, bytestring, containers, lib
, QuickCheck, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "futhark-manifest";
  version = "1.11.0.0";
  sha256 = "56982e85b6cfa3d360088a23142f82d1a4e511babb2d553767dfaa759ae9f682";
  libraryHaskellDepends = [ aeson base bytestring containers text ];
  testHaskellDepends = [
    base QuickCheck quickcheck-instances tasty tasty-hunit
    tasty-quickcheck text
  ];
  doCheck = false;
  description = "Definition and serialisation instances for Futhark manifests";
  license = lib.meta.getLicenseFromSpdxId "ISC";
}
