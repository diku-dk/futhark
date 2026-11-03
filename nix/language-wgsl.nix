{ mkDerivation, base, lib, prettyprinter, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "language-wgsl";
  version = "0.1.0.0";
  sha256 = "57606335a77630060593dba43f97bea3d5f0015813b0adee4aa2ab4071209cbc";
  libraryHaskellDepends = [ base prettyprinter text ];
  testHaskellDepends = [ base prettyprinter tasty tasty-hunit text ];
  homepage = "https://github.com/diku-dk/language-wgsl";
  description = "AST, prettyprinter, and other tools for the WebGPU Shading Language";
  license = lib.meta.getLicenseFromSpdxId "ISC";
}
