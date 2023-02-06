{
  description = "Backend for declarative.tv";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      declarative-tv-backend = pkgs.callPackage ./declarative-tv-backend.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
      };
      defaultPackage = declarative-tv-backend;
      packages = flake-utils.lib.flattenTree {
        inherit declarative-tv-backend;
      };
    });
}
