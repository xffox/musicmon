{
  description = "musicmon";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        musicmonPkg = pkgs.callPackage ./nix/musicmon.nix { };
        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          programs.nixfmt.enable = true;
          programs.fourmolu.enable = true;
          projectRootFile = "flake.nix";
        };
      in
      {
        packages.default = musicmonPkg;
        devShells.default = pkgs.callPackage ./nix/shell.nix { musicmon = musicmonPkg; };
        formatter = treefmtEval.config.build.wrapper;
        checks = {
          formatting = treefmtEval.config.build.check self;
        };
      }
    );
}
