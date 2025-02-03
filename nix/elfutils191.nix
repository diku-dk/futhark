{ lib, stdenv, fetchurl, fetchpatch, pkg-config, musl-fts
, musl-obstack, m4, zlib, zstd, bzip2, bison, flex, gettext, xz, setupDebugInfoDirs
, argp-standalone
, enableDebuginfod ? lib.meta.availableOn stdenv.hostPlatform libarchive, sqlite, curl, libmicrohttpd, libarchive
, gitUpdater, autoreconfHook
}:

# TODO: Look at the hardcoded paths to kernel, modules etc.
stdenv.mkDerivation rec {
  pname = "elfutils";
  version = "0.191";

  src = fetchurl {
    url = "https://sourceware.org/elfutils/ftp/${version}/${pname}-${version}.tar.bz2";
    hash = "sha256-33bbcTZtHXCDZfx6bGDKSDmPFDZ+sriVTvyIlxR62HE=";
  };

  postPatch = ''
    patchShebangs tests/*.sh
  '' + lib.optionalString stdenv.hostPlatform.isRiscV ''
    # disable failing test:
    #
    # > dwfl_thread_getframes: No DWARF information found
    sed -i s/run-backtrace-dwarf.sh//g tests/Makefile.in
  '';

  outputs = [ "bin" "dev" "out" "man" ];

  # We need bzip2 in NativeInputs because otherwise we can't unpack the src,
  # as the host-bzip2 will be in the path.
  nativeBuildInputs = [ m4 bison flex gettext bzip2 ]
    ++ lib.optional enableDebuginfod pkg-config
    ++ lib.optional (stdenv.targetPlatform.useLLVM or false) autoreconfHook;
  buildInputs = [ zlib zstd bzip2 xz ]
    ++ lib.optionals stdenv.hostPlatform.isMusl [
    argp-standalone
    musl-fts
    musl-obstack
  ] ++ lib.optionals enableDebuginfod [
    sqlite
    curl
    libmicrohttpd
    libarchive
  ];

  propagatedNativeBuildInputs = [ setupDebugInfoDirs ];

  configureFlags = [
    "--program-prefix=eu-" # prevent collisions with binutils
    "--enable-deterministic-archives"
    (lib.enableFeature enableDebuginfod "libdebuginfod")
    (lib.enableFeature enableDebuginfod "debuginfod")

    # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101766
    # Versioned symbols are nice to have, but we can do without.
    (lib.enableFeature (!stdenv.hostPlatform.isMicroBlaze) "symbol-versioning")
  ] ++ lib.optional (stdenv.targetPlatform.useLLVM or false) "--disable-demangler"
    ++ lib.optionals stdenv.cc.isClang [
      "CFLAGS=-Wno-unused-private-field"
      "CXXFLAGS=-Wno-unused-private-field"
    ];

  enableParallelBuilding = true;


  doCheck =
    # Backtrace unwinding tests rely on glibc-internal symbol names.
    # Musl provides slightly different forms and fails.
    # Let's disable tests there until musl support is fully upstreamed.
    !stdenv.hostPlatform.isMusl
    # Test suite tries using `uname` to determine whether certain tests
    # can be executed, so we need to match build and host platform exactly.
    && (stdenv.hostPlatform == stdenv.buildPlatform);
  doInstallCheck = !stdenv.hostPlatform.isMusl
    && (stdenv.hostPlatform == stdenv.buildPlatform);

  passthru.updateScript = gitUpdater {
    url = "https://sourceware.org/git/elfutils.git";
    rev-prefix = "elfutils-";
  };

  meta = with lib; {
    homepage = "https://sourceware.org/elfutils/";
    description = "Set of utilities to handle ELF objects";
    platforms = platforms.linux;
    # https://lists.fedorahosted.org/pipermail/elfutils-devel/2014-November/004223.html
    badPlatforms = [ lib.systems.inspect.platformPatterns.isStatic ];
    # licenses are GPL2 or LGPL3+ for libraries, GPL3+ for bins,
    # but since this package isn't split that way, all three are listed.
    license = with licenses; [ gpl2Only lgpl3Plus gpl3Plus ];
    maintainers = with maintainers; [ r-burns ];
  };
}
