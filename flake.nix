# This flake.nix flake contains the following outputs:
#
#  * 'futhark' (the default) is a derivation with a statically linked 'futhark'
#    binary and manpages.
#
#  * 'tarball' produces a release tarball; essentially just the 'futhark'
#    derivation packaged up.
#
#  * 'tarball-nightly' produces a release tarball where the version number is "nightly".
#
# Just run
#
#   nix build
#
# and look in 'result'.
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
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      lib = nixpkgs.lib;

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = lib.genAttrs supportedSystems;

      commit = self.rev or "unknown";

      # --- Source filtering (shared) ---
      cleanSource =
        src:
        lib.sourceByRegex src [
          "futhark.cabal"
          "Setup.hs"
          "LICENSE"
          "^src.*"
          "^rts.*"
          "^docs.*"
          "^prelude.*"
          "^assets.*"
          "^src-testing.*"
          "^tools.*"
        ];
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          python = pkgs.python313Packages;
          haskell = pkgs.haskell.packages.ghc910;

          # The Nix 'system' convention is unfortunately the opposite of what we use
          # in our tarballs, so flip them around here.
          platform =
            lib.replaceStrings
              [
                "x86_64-linux"
                "aarch64-linux"
                "x86_64-darwin"
                "aarch64-darwin"
              ]
              [
                "linux-x86_64"
                "linux-aarch64"
                "x86_darwin-64"
                "darwin-aarch64"
              ]
              system;

          pkgs' = import pkgs.path {
            inherit system;

            config.packageOverrides = pkgs: {
              haskellPackages = pkgs.haskellPackages.override {
                overrides = new: old: {
                  # Custom dependencies for which Nixpkgs is too old.
                  lsp = new.callPackage ./nix/lsp.nix { };
                  lsp-types = new.callPackage ./nix/lsp-types.nix { };
                  lsp-test = new.callPackage ./nix/lsp-test.nix { };
                  futhark-data = new.callPackage ./nix/futhark-data.nix { };
                  futhark-server = new.callPackage ./nix/futhark-server.nix { };
                  futhark-manifest = new.callPackage ./nix/futhark-manifest.nix { };

                  # This derivation builds a statically linked 'futhark'
                  # executable on Linux; on other platforms it uses the default
                  # shared-library build.
                  futhark =
                    let
                      base = new.callCabal2nix "futhark" (cleanSource ./.) { };
                    in
                    pkgs.haskell.lib.overrideCabal base (old: {
                      enableLibraryProfiling = false;
                      isExecutable = true;
                      isLibrary = false;

                      enableSharedExecutables = !pkgs.stdenv.isLinux;
                      enableSharedLibraries = !pkgs.stdenv.isLinux;

                      configureFlags = [
                        "--ghc-option=-Werror"
                        "--ghc-option=-split-sections"
                      ]
                      ++ lib.optionals pkgs.stdenv.isLinux (
                        [
                          # static linking
                          "--ghc-option=-optl=-static"
                          "--ghc-option=-optl=-lbz2"
                          "--ghc-option=-optl=-lz"
                          "--ghc-option=-optl=-lelf"
                          "--ghc-option=-optl=-llzma"
                          "--ghc-option=-optl=-lzstd"
                        ]
                        ++ map (p: "--extra-lib-dirs=${p}/lib") [
                          pkgs.glibc.static
                          (pkgs.ncurses.override { enableStatic = true; })
                          (pkgs.gmp6.override { withStatic = true; })
                          (pkgs.libffi.overrideAttrs { dontDisableStatic = true; })
                          (pkgs.numactl.overrideAttrs { dontDisableStatic = true; })
                          pkgs.zlib.static
                          (pkgs.xz.override { enableStatic = true; }).out
                          (pkgs.zstd.override { enableStatic = true; }).out
                          (pkgs.bzip2.override { enableStatic = true; }).out
                          (pkgs.elfutils.overrideAttrs { dontDisableStatic = true; }).out
                        ]
                      );

                      preBuild = "echo ${commit} > commit-id";
                    });
                };
              };
            };
          };

          # Compiled Futhark binary with documentation, parameterised over the
          # version number in the file names.
          futhark =
            version:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "futhark";
              version = version;
              src = cleanSource ./.;

              nativeBuildInputs = [
                pkgs'.haskellPackages.futhark
                python.sphinx
              ];

              buildPhase = ''
                make -C docs man
              '';

              installPhase = ''
                mkdir -p $out
                mkdir -p $out/share/man/man1

                cp -r tools/release/skeleton/* $out/
                cp -r ${pkgs'.haskellPackages.futhark}/bin $out/bin
                cp -r docs/_build/man/*.1 $out/share/man/man1/
                cp LICENSE $out/
                echo "${commit}" > $out/commit-id
              '';
            };

          tarball =
            futhark:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "futhark-tarball";
              version = futhark.version;
              src = false;

              dontUnpack = true;

              nativeBuildInputs = [
                futhark
              ];

              buildPhase = ''
                mkdir -p futhark-${futhark.version}-${platform}
                cp -r ${futhark}/* futhark-${futhark.version}-${platform}/
                tar -c -f futhark-${futhark.version}-${platform}.tar.xz -J --mode 'u+w' futhark-${futhark.version}-${platform}
              '';

              installPhase = ''
                mkdir -p $out
                cp futhark-${futhark.version}-${platform}.tar.xz $out/
              '';
            };

          # Extract version number from Futhark derivation.
          versionFromCabal = pkgs'.haskellPackages.futhark.version;
        in
        {
          futhark = futhark versionFromCabal;

          tarball = tarball (futhark versionFromCabal);

          tarball-nightly = tarball (futhark "nightly");

          # default package
          default = futhark versionFromCabal;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          python = pkgs.python313Packages;
          haskell = pkgs.haskell.packages.ghc910;
        in
        {
          # A shell that only contains tools for style checking.
          style = pkgs.mkShell {
            nativeBuildInputs = (import ./nix/pkgs-style.nix { inherit pkgs haskell python; });
          };

          # A shell for running tests in CI.
          test = pkgs.mkShell {
            nativeBuildInputs = import ./nix/pkgs-test.nix { inherit pkgs python; };
          };

          # The main development shell that contains all development tools.
          default = pkgs.mkShell {
            nativeBuildInputs =
              (import ./nix/pkgs-style.nix { inherit pkgs haskell python; })
              ++ (
                with pkgs;
                [
                  cabal-install
                  cacert
                  curl
                  file
                  git
                  haskell.ghc
                  haskell.weeder
                  haskell.haskell-language-server
                  haskellPackages.graphmod
                  #      haskellPackages.apply-refact
                  xdot
                  pkg-config
                  zlib
                  zlib.out
                  cabal2nix
                  nixfmt
                  ghcid
                  niv
                  ispc
                  python.python
                  python.mypy
                  python.black
                  python.cycler
                  python.numpy
                  python.pyopencl
                  python.matplotlib
                  python.jsonschema
                  python.sphinx
                  (python.sphinxcontrib-bibtex.overridePythonAttrs (_: {
                    doCheck = false;
                  }))
                  imagemagick # needed for literate tests
                ]
                ++ lib.optionals (stdenv.isLinux) [
                  opencl-headers
                  ocl-icd
                  (callPackage ./nix/oclgrind.nix { })
                  rocmPackages.clr
                ]
              );
          };
        }
      );
    };
}
