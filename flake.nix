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

    effectful-libs = {
      url = "github:tbidne/effectful-libs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
    };

    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };

    smart-math = {
      url = "github:tbidne/smart-math";
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
          ghc-version = "ghc9122";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                Cabal-syntax_3_10_3_0 = hlib.doJailbreak prev.Cabal-syntax_3_10_3_0;

                # These tests are flaky.
                auto-update = hlib.dontCheck prev.auto-update;
                effectful = hlib.dontCheck prev.effectful;
                statistics = hlib.dontCheck prev.statistics;
                tasty-bench-fit = hlib.dontCheck prev.tasty-bench-fit;
                warp = hlib.dontCheck prev.warp;

                # TODO: Would be great to be able to remove this (when it's
                # the default in nixpkgs) as overriding forces a rebuild of
                # many packages, hence makes the dev shell slow.
                fourmolu = hlib.doJailbreak prev.fourmolu;
                hspec-golden = hlib.doJailbreak prev.hspec-golden;
                ormolu = hlib.doJailbreak prev.ormolu;
                optparse-applicative = prev.optparse-applicative_0_19_0_0;
                stylish-haskell = hlib.doJailbreak prev.stylish-haskell;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "smart-math"
                "relative-time"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.effectful-libs}/lib" final [
                "concurrent-effectful"
                "effectful-utils"
                "fs-effectful"
                "ioref-effectful"
                "logger-effectful"
                "optparse-effectful"
                "terminal-effectful"
                "time-effectful"
              ];
          };
          compilerPkgs = {
            inherit compiler pkgs;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };

          # Set in ci.yaml and Dockerfiles. To ensure consistency, we should
          # choose whatever version is in our docker image's repo.
          # See NOTE: [Alpine and nodejs].
          node = pkgs.nodejs_22;

          # Overriding buildNpmPackage just so we can explicitly set the
          # nodejs version. Make sure this doesn't hurt caching.
          buildNpmPackage = pkgs.buildNpmPackage.override {
            nodejs = node;
          };

          frontend = buildNpmPackage {
            name = "frontend";
            src = ./web;
            npmDepsHash = "sha256-Bcyl4cOM3lpJWPdCAk0kczC7d9NFMM9UQvqUjGga6co=";
          };

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "pacer";
              root = ./.;

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  PACER_HASH = "${self.rev or self.dirtyRev}";
                  PACER_MODIFIED = "${builtins.toString self.lastModified}";
                  PACER_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";

                  # The haskell exe requires the nodejs generated dist/* files
                  # at build time. Hence we have to build them first then copy
                  # them over.
                  postUnpack = ''
                    mkdir -p $sourceRoot/web/dist
                    cp ${frontend}/lib/node_modules/pacer/dist/* $sourceRoot/web/dist
                  '';

                  # Git is needed to run the tests (git diff).
                  nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ] ++ [
                    frontend
                    pkgs.git
                  ];
                });

              devTools = nix-hs-utils.mkDevTools compilerPkgs ++ webDeps;
            };

          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
          };

          webDeps = [ node ];
        in
        {
          packages = {
            inherit frontend;
            default = mkPkg false;

            # This enables the e2e tests. Note that it currently fails, due
            # to trying to write to the home directory.
            e2e = (mkPkg false).overrideAttrs {
              RUN_E2E = "1";
            };
          };

          devShells = {
            default = mkPkg true;

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (compilerPkgs // pkgsMkDrv))
                ({
                  inherit pkgs;
                  name = "format";
                  text = "prettier -w -- **/*html **/*js **/*json **/*ts **/*yaml";
                  runtimeInputs = [ pkgs.nodePackages.prettier ];
                })
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
