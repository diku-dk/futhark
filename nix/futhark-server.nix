{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.5.0.0";
  sha256 = "99d6a34a7157e1eabe1c9639c700ea914b442df337b280dc81472dfbd48fbe7d";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  doCheck = false;
  description = "Client implementation of the Futhark server protocol";
  license = lib.licensesSpdx."ISC";
}
