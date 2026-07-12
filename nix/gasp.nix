{ mkDerivation, adjunctions, base, binary, constraints, containers
, distributive, lib, mtl, QuickCheck
}:
mkDerivation {
  pname = "gasp";
  version = "1.4.0.0";
  sha256 = "9a73a6ea7eb844493deb76c85c50249915e5ca29a6734a0b133a0e136c232f9f";
  libraryHaskellDepends = [
    adjunctions base binary constraints containers distributive mtl
    QuickCheck
  ];
  description = "A framework of algebraic classes";
  license = lib.licenses.bsd3;
}
