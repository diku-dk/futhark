{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.2.4.0";
  sha256 = "a833f68d6fb03cfca2f71f0fc7d5ab8b2b2373f7f53c70858d96fd2529ab7e3f";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
