{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.4.1.1";
  sha256 = "6222b80c39ed06b8bd5274d1c82b4cc1d3c47c21253d28b92b74223ac663b2a2";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licensesSpdx."ISC";
}
