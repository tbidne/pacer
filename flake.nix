{
  description = "Pacer: A utility for runners.";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };

    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              { }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "smart-math"
                "relative-time"
              ];
          };
          compilerPkgs = {
            inherit compiler pkgs;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "pacer";
              root = ./.;
            };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;

            nodejs = pkgs.mkShell {
              buildInputs = [
                pkgs.nodejs_23
              ];
            };
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.lint (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
