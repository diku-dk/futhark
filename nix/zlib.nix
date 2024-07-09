{ mkDerivation, base, bytestring, lib, QuickCheck, tasty
, tasty-quickcheck, zlib
}:
mkDerivation {
  pname = "zlib";
  version = "0.7.0.0";
  sha256 = "7e43c205e1e1ff5a4b033086ec8cce82ab658879e977c8ba02a6701946ff7a47";
  libraryHaskellDepends = [ base bytestring ];
  libraryPkgconfigDepends = [ zlib ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-quickcheck
  ];
  description = "Compression and decompression in the gzip and zlib formats";
  license = lib.licenses.bsd3;
}
