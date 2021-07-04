{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.1.0.0";
  sha256 = "0mv3q4a6l3xp0qjlhh9f8bvgbmrmr4hypnkapb2wsn0fvb0iw2kb";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
