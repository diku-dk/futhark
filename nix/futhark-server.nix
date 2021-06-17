{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.0.0.0";
  sha256 = "4a6d233f0e234fdd379e0d6b766b6dc3c4c4175c5afd12a6354f9bbee8c26001";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
