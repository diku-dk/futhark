{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/93080703881acdfebffd39d9dc5e2031b3efd4db";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { nixpkgs, flake-utils, ... }@inputs: 
  flake-utils.lib.eachDefaultSystem (system: rec {
    packages = flake-utils.lib.flattenTree (import ./packages.nix {
      inherit system inputs;
      pkgs = import nixpkgs { inherit system; };
    });
    defaultPackage = packages.release;

    apps.futhark = flake-utils.lib.mkApp { drv = packages.futhark; };
    defaultApp = apps.futhark;

    devShell = import ./shell.nix {
      pkgs = nixpkgs.legacyPackages.${system};
    };
  });
}
