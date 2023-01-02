# This default.nix builds a tarball containing a statically linked
# Futhark binary and some manpages.  Likely to only work on linux.
#
# Just run 'nix-build' and fish the tarball out of 'result/'.
#
# For the Haskell dependencies that diverge from our pinned Nixpkgs,
# we use cabal2nix like thus:
#
#  $ cabal2nix cabal://sexp-grammar-2.2.1 > nix/sexp-grammar.nix
#
# And then import them into the configuration.  Although note that
# Nixpkgs also tends to contain the newest version of each Hackage
# package, even if it is not the default.
#
# To update the Nixpkgs snapshot (which also includes tooling), use:
#
#  $ niv update nixpkgs -b master
#
# Also remember this guide: https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md

{ commit ? "" }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          futhark-data =
            haskellPackagesNew.callPackage ./nix/futhark-data.nix { };

          futhark-server =
            haskellPackagesNew.callPackage ./nix/futhark-server.nix { };

          futhark-manifest =
            haskellPackagesNew.callPackage ./nix/futhark-manifest.nix { };

          lsp =
            haskellPackagesOld.lsp_1_5_0_0;

          lsp-types =
            haskellPackagesOld.lsp-types_1_5_0_0;

          futhark =
            # callCabal2Nix does not do a great job at determining
            # which files must be included as source, which causes
            # trouble if you have lots of other large files lying
            # around (say, data files for testing).  As a workaround
            # we explicitly tell it which files are needed.  This must
            # be _manually_ kept in sync with whatever the cabal file requires.
            let sources = ["futhark.cabal"
                           "Setup.hs"
                           "LICENSE"
                           "^src.*"
                           "^rts.*"
                           "^docs.*"
                           "^prelude.*"
                           "^assets.*"
                           "^unittests.*"
                          ];
                cleanSource = src: pkgs.lib.sourceByRegex src sources;
            in
            pkgs.haskell.lib.overrideCabal
              (pkgs.haskell.lib.addBuildTools
                (haskellPackagesOld.callCabal2nix "futhark" (cleanSource ./.) { })
                [ pkgs.python39Packages.sphinx ])
              ( _drv: {
                isLibrary = false;
                isExecutable = true;
                enableSharedExecutables = false;
                enableSharedLibraries = false;
                enableLibraryProfiling = false;
                configureFlags = [
                  "--ghc-option=-Werror"
                  "--ghc-option=-optl=-static"
                  "--ghc-option=-split-sections"
                  "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
                  "--extra-lib-dirs=${pkgs.glibc.static}/lib"
                  "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                  "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                ];

                preBuild = ''
        echo "${self.rev or "dirty"}" > commit-id
                '';

                postBuild = (_drv.postBuild or "") + ''
        make -C docs man
        '';

                postInstall = (_drv.postInstall or "") + ''
        mkdir -p $out/share/man/man1
        cp docs/_build/man/*.1 $out/share/man/man1/
        mkdir -p $out/share/futhark/
        cp LICENSE $out/share/futhark/
        '';
              }
              );
        };
      };
    };
  };

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };

  futhark = pkgs.haskellPackages.futhark;

in pkgs.stdenv.mkDerivation rec {
  name = "futhark";
  version = futhark.version;
  src = tools/release;

  buildInputs = [ futhark ];

  buildPhase = ''
    cp -r skeleton futhark
    cp -r ${futhark}/bin futhark/bin
    mkdir -p futhark/share
    cp -r ${futhark}/share/man futhark/share/
    chmod +w -R futhark
    cp ${futhark}/share/futhark/LICENSE futhark/
  '';

  # installPhase = ''
  #   mkdir -p $out
  #   cp futhark.tar.xz $out/futhark.tar.xz
  # '';
}
