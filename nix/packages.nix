{ inputs, system, pkgs, commit ? "", suffix ? "nightly", ... }:

let
  futhark-config = import ./config.nix { inherit commit; };
  futhark-pkgs = import inputs.nixpkgs {
    inherit system;
    config = futhark-config;
  };
in
rec {
  futhark = futhark-pkgs.callPackage ./futhark.nix { };
  release = futhark.overrideAttrs (oldAttrs: {
    postBuild = ''
      mv futhark futhark-${suffix}
      tar -Jcf futhark-${suffix}.tar.xz futhark-${suffix}
    '';
    installPhase = ''
      mkdir -p $out
      cp futhark-${suffix}.tar.xz $out
    '';
  });
}
