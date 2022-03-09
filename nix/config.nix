{ commit ? "", ...}:
{
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        aeson =
          haskellPackagesNew.aeson_2_0_2_0;

        time-compat =
          haskellPackagesNew.time-compat_1_9_6_1;

        semialign =
          haskellPackagesNew.semialign_1_2_0_1;

        hashable =
          haskellPackagesNew.hashable_1_4_0_1;

        OneTuple =
          haskellPackagesNew.OneTuple_0_3_1;

        # Need to disable the test suite as otherwise we have a
        # circular dependency with quickcheck-instances.
        text-short =
          pkgs.haskell.lib.dontCheck haskellPackagesNew.text-short_0_1_4;

        quickcheck-instances =
          haskellPackagesNew.quickcheck-instances_0_3_27;

        hashable-time =
          haskellPackagesNew.hashable-time_0_3;

        futhark-data =
          haskellPackagesNew.callPackage ./futhark-data.nix { };

        futhark-server =
          haskellPackagesNew.callPackage ./futhark-server.nix { };

        futhark-manifest =
          haskellPackagesNew.callPackage ./futhark-manifest.nix { };

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
              (haskellPackagesOld.callCabal2nix "futhark" (cleanSource ../.) { })
              [ pkgs.python39Packages.sphinx ])
            ( _drv: {
              isLibrary = false;
              isExecutable = true;
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              enableLibraryProfiling = false;
              configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-split-sections"
                "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
                "--extra-lib-dirs=${pkgs.glibc.static}/lib"
                "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              ];

              preBuild = ''
      if [ "${commit}" ]; then echo "${commit}" > commit-id; fi
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
}
