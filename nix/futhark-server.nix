{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.2.2.1";
  sha256 = "5845ca6f4dd8395ef860164a7e8193e4094dc342a5b7187e314a7842e70b163d";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
