{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.3.3.0";
  sha256 = "b239d909a9b808a4eef62f0c98da811668bfda4bf6ad6fad01fed7f01c658548";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  doCheck = false;
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
