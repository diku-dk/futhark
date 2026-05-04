# This default.nix builds a tarball containing a statically linked Futhark
# binary and some manpages.  Likely to only work on linux.
#
# Just run 'nix-build' and fish the tarball out of 'result/'.
#
# For the Haskell dependencies that diverge from our pinned Nixpkgs, we use
# cabal2nix like thus:
#
#  $ cabal2nix cabal://sexp-grammar-2.2.1 > nix/sexp-grammar.nix
#
# And then import them into the configuration.  Although note that Nixpkgs also
# tends to contain the newest version of each Hackage package, even if it is not
# the default.
#
# To update the Nixpkgs snapshot (which also includes tooling), use:
#
#  $ nix flake update
#
# Also remember this guide: https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md

{
  pkgs ? import (import ./nix/sources.nix).nixpkgs { },
  commit ? "",
  suffix ? "nightly",
}:
let
  inherit (pkgs.lib)
    sourceByRegex
    ;

  # 'callCabal2nix' does not do a great job at determining which files must be
  # included as source, which causes trouble if you have lots of other large
  # files lying around (say, data files for testing).
  # As a workaround we explicitly tell it which files are needed.  This must be
  # _manually_ kept in sync with whatever the cabal file requires.
  cleanSource =
    src:
    sourceByRegex src [
      "futhark.cabal"
      "Setup.hs"
      "LICENSE"
      "^src.*"
      "^rts.*"
      "^docs.*"
      "^prelude.*"
      "^assets.*"
      "^src-testing.*"
    ];

  pkgs' = import pkgs.path {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: {
          # Dependencies for build.
          lsp = new.callPackage ./nix/lsp.nix { };
          lsp-types = new.callPackage ./nix/lsp-types.nix { };
          lsp-test = new.callPackage ./nix/lsp-test.nix { };
          futhark-data = new.callPackage ./nix/futhark-data.nix { };
          futhark-server = new.callPackage ./nix/futhark-server.nix { };
          futhark-manifest = new.callPackage ./nix/futhark-manifest.nix { };

          # Normal build.
          futhark =
            let
              build-unconfigured = new.callCabal2nix "futhark" (cleanSource ./.) { };
              build-with-sphinx = pkgs.haskell.lib.addBuildTools build-unconfigured [
                pkgs.python312Packages.sphinx
              ];
            in
            pkgs.haskell.lib.overrideCabal build-with-sphinx (oldAttrs: {
              enableLibraryProfiling = false;
              isExecutable = true;
              isLibrary = false;

              configureFlags = [
                "--ghc-option=-Werror"
                "--ghc-option=-split-sections"
              ];

              preBuild = ''
                if [ "${commit}" ]; then echo "${commit}" > commit-id; fi
              '';

              postBuild = oldAttrs.postBuild or "" + ''
                make -C docs man
              '';

              postInstall = oldAttrs.postInstall or "" + ''
                mkdir -p $out/share/man/man1
                cp docs/_build/man/*.1 $out/share/man/man1/
                mkdir -p $out/share/futhark/
                cp LICENSE $out/share/futhark/
              '';
            });

          # Overriding normal build to be statically linked.
          futhark-static = pkgs.haskell.lib.overrideCabal new.futhark (oldAttrs: {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags =
              oldAttrs.configureFlags or [ ]
              ++ [
                # Static linking crud
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-lbz2"
                "--ghc-option=-optl=-lz"
                "--ghc-option=-optl=-lelf"
                "--ghc-option=-optl=-llzma"
                "--ghc-option=-optl=-lzstd"
              ]
              ++ map (path: "--extra-lib-dirs=${path}/lib") [
                pkgs.glibc.static
                (pkgs.ncurses.override { enableStatic = true; })
                (pkgs.gmp6.override { withStatic = true; })
                (pkgs.libffi.overrideAttrs { dontDisableStatic = true; })
                (pkgs.numactl.overrideAttrs { dontDisableStatic = true; })
                # The ones below are due to GHC'.pkgsStatics runtime system
                # depending on libdw (DWARF info), which depends on a bunch of
                # compression algorithms.
                pkgs.zlib.static
                (pkgs.xz.override { enableStatic = true; }).out
                (pkgs.zstd.override { enableStatic = true; }).out
                (pkgs.bzip2.override { enableStatic = true; }).out
                (pkgs.elfutils.overrideAttrs { dontDisableStatic = true; }).out
              ];
          });
        };
      };
    };
  };
in
pkgs'.callPackage (
  {
    haskellPackages,
    stdenvNoCC,
    gnutar,
    ...
  }:
  let
    inherit (haskellPackages) futhark-static;
  in
  stdenvNoCC.mkDerivation {
    name = "futhark-" + suffix;
    version = futhark-static.version;
    src = ./tools/release;

    nativeBuildInputs = [
      futhark-static
      gnutar
    ];

    buildPhase = ''
      cp -r skeleton futhark-${suffix}
      cp -r ${futhark-static}/bin futhark-${suffix}/bin
      mkdir -p futhark-${suffix}/share
      cp -r ${futhark-static}/share/man futhark-${suffix}/share/
      chmod +w -R futhark-${suffix}
      cp ${futhark-static}/share/futhark/LICENSE futhark-${suffix}/
    '';

    installPhase = ''
      mkdir $out
      tar -cJf $out/futhark-${suffix}.tar.xz futhark-${suffix}
    '';

    passthru = {
      inherit (haskellPackages) futhark futhark-static;
    };
  }
) { }
