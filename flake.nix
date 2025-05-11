{
  description = "musicmon";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        musicmonPkg = pkgs.callPackage ./nix/musicmon.nix { };
      in {
        packages.default = musicmonPkg;
        devShells.default = pkgs.callPackage ./nix/shell.nix { };
      });
}
