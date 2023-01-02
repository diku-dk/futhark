{
  description = "Futhark";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/454e81780251b7d241a150e941e73573dbca25bc";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
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

                futhark-static =
                  # callCabal2Nix does not do a great job at determining
                  # which files must be included as source, which causes
                  # trouble if you have lots of other large files lying
                  # around (say, data files for testing).  As a workaround
                  # we explicitly tell it which files are needed.  This must
                  # be _manually_ kept in sync with whatever the cabal file requires.
                  let
                    sources = [
                      "futhark.cabal"
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
                  in pkgs.haskell.lib.overrideCabal
                  (pkgs.haskell.lib.addBuildTools
                    (haskellPackagesOld.callCabal2nix "futhark"
                      (cleanSource ./.) { }) [ pkgs.python39Packages.sphinx ])
                  (_drv: {
                    isLibrary = false;
                    isExecutable = true;
                    enableSharedExecutables = false;
                    enableSharedLibraries = false;
                    enableLibraryProfiling = false;
                    configureFlags = [
                      "--ghc-option=-Werror"
                      "--ghc-option=-optl=-static"
                      "--ghc-option=-split-sections"
                      "--extra-lib-dirs=${
                        pkgs.ncurses.override { enableStatic = true; }
                      }/lib"
                      "--extra-lib-dirs=${pkgs.glibc.static}/lib"
                      "--extra-lib-dirs=${
                        pkgs.gmp6.override { withStatic = true; }
                      }/lib"
                      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                      "--extra-lib-dirs=${
                        pkgs.libffi.overrideAttrs
                        (old: { dontDisableStatic = true; })
                      }/lib"
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
                  });
              };
            };
          };
        };

        # pkgs = nixpkgs.legacyPackages.${system};

        pkgs = import nixpkgs {
          system = "${system}";
          inherit config;
        };

        futhark = pkgs.haskellPackages.futhark-static;

        futhark-release = pkgs.stdenv.mkDerivation rec {
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
            echo "${self.rev or "dirty"}" > futhark/commit-id
            tar -Jcf futhark.tar.xz futhark
          '';

          installPhase = ''
            mkdir -p $out
            cp futhark.tar.xz $out/futhark.tar.xz
          '';
        };

      in rec {
        packages.futhark-static = futhark;
        packages.futhark-release = futhark-release;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              cabal-install
              cacert
              curl
              file
              git
              parallel
              haskell.compiler.ghc924
              haskell.packages.ghc924.ormolu
              haskell.packages.ghc924.weeder
              haskell.packages.ghc924.haskell-language-server
              haskellPackages.graphmod
              haskellPackages.apply-refact
              xdot
              hlint
              pkgconfig
              zlib
              zlib.out
              cabal2nix
              ghcid
              niv
              ispc
              python3Packages.numpy
              python3Packages.pyopencl
              python3Packages.jsonschema
              python3Packages.sphinx
              python3Packages.sphinxcontrib-bibtex
              imagemagick # needed for literate tests
              ffmpeg
            ] ++ # lib.optionals (stdenv.isLinux)
            [ opencl-headers ocl-icd oclgrind ];
        };
      });

}
