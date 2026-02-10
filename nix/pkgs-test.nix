# Packages needed to test the compiler in CI, but not to build it.
{ pkgs, python }:
with pkgs;
[
  parallel

  ispc

  python.jsonschema
  python.numpy
  python.pyopencl

  imagemagick # needed for literate tests

  opencl-headers
  ocl-icd
  (pkgs.callPackage ./oclgrind.nix {})
]
