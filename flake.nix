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
                effectful-core = prev.effectful-core_2_5_1_0;
                effectful = prev.effectful_2_5_1_0;
                megaparsec = prev.megaparsec_9_7_0;
                path = hlib.dontCheck prev.path_0_9_6;
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
                "fs-effectful"
                "ioref-effectful"
                "logger-effectful"
                "logger-ns-effectful"
                "optparse-effectful"
                "terminal-effectful"
                "time-effectful"
                "typed-process-dynamic-effectful"
              ];
          };
          compilerPkgs = {
            inherit compiler pkgs;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
          node = pkgs.nodejs_23;

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "pacer";
              root = ./.;

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  # So wrapProgram is available in postFixup
                  nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];

                  # This is apparently unnecessary, but knowing what version of
                  # node pacer is using could be helpful.
                  installPhase =
                    oldAttrs.installPhase
                    + ''
                      ln -s ${node.out}/bin/npm $out/bin/npm
                    '';
                  postFixup = ''
                    wrapProgram $out/bin/pacer --prefix PATH : ${pkgs.lib.makeBinPath webDeps}
                  '';
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
          packages.default = mkPkg false;

          devShells = {
            default = mkPkg true;

            # Blank shell intended to be used with --unset PATH, to test that
            # we are correctly bundling node dep (default github image has
            # node, but we want to ensure we are using __our__ dep).
            ci = pkgs.mkShell {
              buildInputs = [ ];
            };

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
