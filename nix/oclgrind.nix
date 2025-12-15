{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  llvmPackages_19,
  readline,
  python3,
}:

stdenv.mkDerivation rec {
  pname = "oclgrind";
  version = "dev";

  src = fetchFromGitHub {
    owner = "jrprice";
    repo = "oclgrind";
    rev = "3372b8d26ec6104c65f782ae2c44b62c87c8a960";
    sha256 = "sha256-+VSVzF55CgNwEZyGfbk5tygunBxe7PMPSiWic9JF/Rk=";
  };

  nativeBuildInputs = [ cmake ];
  nativeCheckInputs = [ python3 ];
  buildInputs = [
    llvmPackages_19.llvm
    llvmPackages_19.clang-unwrapped
    readline
  ];

  cmakeFlags = [
    "-DCLANG_ROOT=${llvmPackages_19.clang-unwrapped}"
    (lib.cmakeBool "CMAKE_SKIP_RPATH" true)
  ];

  meta = with lib; {
    description = "OpenCL device simulator and debugger";
    homepage = "https://github.com/jrprice/oclgrind";
    license = licenses.bsd3;
    platforms = platforms.linux;
    maintainers = with maintainers; [ athas ];
  };
}
