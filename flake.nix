{
  description = "A Template for Haskell Packages";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      nix-hs-utils,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}";
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
              name = "run-calc";
              root = ./.;
            };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;
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
