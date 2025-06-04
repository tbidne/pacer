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
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                # These tests are flaky.
                auto-update = hlib.dontCheck prev.auto-update;

                megaparsec = prev.megaparsec_9_7_0;
                path = hlib.dontCheck prev.path_0_9_6;
                regression-simple = hlib.doJailbreak prev.regression-simple;

                gitrev-typed = (
                  final.callHackageDirect {
                    pkg = "gitrev-typed";
                    ver = "0.1";
                    sha256 = "sha256-s7LEekR7NLe3CNhD/8uChnh50eGfaArrrtc5hoCtJ1A=";
                  } { }
                );
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
                "logger-ns-effectful"
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
          node = pkgs.nodejs_22;

          # Overriding buildNpmPackage just so we can explicitly set the
          # nodejs version. Make sure this doesn't hurt caching.
          buildNpmPackage = pkgs.buildNpmPackage.override {
            nodejs = node;
          };

          frontend = buildNpmPackage {
            name = "frontend";
            src = ./web;
            npmDepsHash = "sha256-YnxX/dwzsOLIb1o34yRawSfVFzMDbXZ45REQiFit05I=";
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

              # TODO: Once hlint is back to working with our GHC we can
              # use nix-hs-utils.mkDevTools ++ webDeps.
              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ] ++ webDeps;
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
                # TODO: We require GHC 9.10+ since we need filepath >= 1.5,
                # but hlint is sadly not compatible yet. Hence it is disabled
                # for now.
                #
                #(nix-hs-utils.lint (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
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
