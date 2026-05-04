{
  description = "The Futhark programming language compiler.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-compat.url = "github:NixOS/flake-compat/master";
  };

  outputs =
    { nixpkgs, ... }:
    let
      inherit (builtins)
        mapAttrs
        foldl'
        ;
    in
    foldl' (
      acc: system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      acc
      // mapAttrs (name: value: acc.${name} or { } // { ${system} = value; }) {
        devShells = {
          default = import ./shell.nix { inherit pkgs; };
          test = import ./shell-test.nix { inherit pkgs; };
          style = import ./shell-style.nix { inherit pkgs; };
        };
        formatter = pkgs.nixfmt-tree;
        packages =
          let
            futhark-tarball = import ./default.nix { inherit pkgs; };
          in
          {
            inherit futhark-tarball;

            inherit (futhark-tarball)
              futhark
              futhark-static
              ;

            default = futhark-tarball;
          };
      }
    ) { } nixpkgs.lib.systems.flakeExposed;
}
