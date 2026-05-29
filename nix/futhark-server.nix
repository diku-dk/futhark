{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.4.1.0";
  sha256 = "68b786f8f841142924e00d55ef2bffc139d076a626ccb21db5bd34c994754a5c";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licensesSpdx."ISC";
}
