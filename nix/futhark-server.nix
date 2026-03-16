{ mkDerivation, base, binary, bytestring, directory, futhark-data
, lib, mtl, process, temporary, text
}:
mkDerivation {
  pname = "futhark-server";
  version = "1.3.2.0";
  sha256 = "07ecac53e36a97eda44780f8a41afb27ab8bfeb386bd6b8dfcf6ad043e68fbe6";
  revision = "1";
  editedCabalFile = "1jfsv31s90prnh5gipyfa533sdivh8r2mxq0pbvq1vjbiwjqd81q";
  libraryHaskellDepends = [
    base binary bytestring directory futhark-data mtl process temporary
    text
  ];
  doCheck = false;
  description = "Client implementation of the Futhark server protocol";
  license = lib.licenses.isc;
}
