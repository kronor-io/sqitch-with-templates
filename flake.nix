{
  description = "sqitch with kronor specific templates";
  nixConfig = {
    extra-substituters = [
      "https://kronor-open.cachix.org"
      "https://pranaysashank.cachix.org"
    ];
    extra-trusted-public-keys = [
      "pranaysashank.cachix.org-1:VeqW46y6BVO74w4ViwzeWqSpDqxuWxtC2DO2zoe9rzc="
      "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8="
    ];
  };
  inputs = {
    git-hooks.url = "github:cachix/git-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    haskell-nix.url = "github:kronor-io/haskell.nix/wip-nixpkgs-ghc";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    kronor-haskell-packages.url = "github:kronor-io/kronor-haskell-packages/repo";
    kronor-haskell-packages.flake = false;
  };
  outputs =
    { self
    , git-hooks
    , nixpkgs
    , haskell-nix
    , hackage-nix
    , kronor-haskell-packages
    }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      mergeAttrs = x: y: x // y;

      foldr = op: nul: list:
        let
          len = builtins.length list;
          fold' = n:
            if n == len
            then nul
            else op (builtins.elemAt list n) (fold' (n + 1));
        in
        fold' 0;

      foldAttrs =
        op:
        nul:
        list_of_attrs:
        foldr
          (n: a:
          foldr
            (name: o:
            o // { ${name} = op n.${name} (a.${name} or nul); }
            )
            a
            (builtins.attrNames n)
          )
          { }
          list_of_attrs;
      eachSystem = f: foldAttrs mergeAttrs { }
        (map (s: builtins.mapAttrs (_: v: { ${s} = v; }) (f s)) supportedSystems);

      githubActionsLib =
        let
          inherit (builtins) attrValues mapAttrs attrNames;
          flatten = list: builtins.foldl' (acc: v: acc ++ v) [ ] list;
        in
        rec {
          githubPlatforms = {
            "x86_64-linux" = "ubuntu-latest";
            "x86_64-darwin" = "macos-13";
            "aarch64-darwin" = "macos-latest";
            "aarch64-linux" = "ubuntu-24.04-arm";
          };

          mkGithubMatrix =
            { checks
            , attrPrefix ? "githubActions.checks"
            , platforms ? githubPlatforms
            }: {
              inherit checks;
              matrix = {
                include = flatten (attrValues (
                  mapAttrs
                    (
                      system: pkgs: builtins.map
                        (attr:
                          {
                            name = attr;
                            inherit system;
                            os =
                              let
                                os = platforms.${system};
                              in
                              if builtins.typeOf os == "list" then os else [ os ];
                            attr = (
                              if attrPrefix != ""
                              then "${attrPrefix}.${system}.\"${attr}\""
                              else "${system}.\"${attr}\""
                            );
                          })
                        (attrNames pkgs)
                    )
                    checks));
              };
            };
        };
    in
    eachSystem
      (system:
      let
        haskellNix = import haskell-nix {
          inherit system;
          sourcesOverride = {
            hackage = hackage-nix;
          };
        };

        sqitchTemplateOverlay = import ./sqitch-template-overlay.nix;

        pkgs = import nixpkgs (
          {
            inherit system;
            overlays = [ sqitchTemplateOverlay haskellNix.overlay ];
          }
        );

        compiler-nix-name = "ghc982";
        index-state = "2025-08-11T23:44:30Z";

        createMigrationProject = pkgs.haskell-nix.project {
          src = ./.;
          inputMap = {
            "https://kronor-io.github.io/kronor-haskell-packages" = kronor-haskell-packages;
          };

          modules = (if system == "x86_64-darwin" || system == "aarch64-darwin" then [ ] else [{
            dontPatchELF = false;
            dontStrip = false;
          }]) ++ [{ doHaddock = false; }];

          inherit compiler-nix-name;
          cabalProjectFreeze = builtins.readFile ./cabal.project.freeze;

          supportHpack = false;
        };

        shell = createMigrationProject.shellFor {
          withHoogle = false;

          nativeBuildInputs = [
            self.packages.${system}.sqitchPg
            self.checks.${system}.pre-commit-check.enabledPackages
          ];

          shellHook = ''
            ${self.checks.${system}.pre-commit-check.shellHook}
          '';
        };

      in
      {
        checks = {
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              fourmolu.enable = true;
              zizmor.enable = true;
            };
          };
        };
        packages = {
          default = createMigrationProject.create-migration.components.exes.create-migration;
          freezeFile = createMigrationProject.plan-nix.freeze;
          sqitchPg = pkgs.sqitchPg;
        };
        devShells.default = shell;
      }
      ) // {
      githubActions = githubActionsLib.mkGithubMatrix {
        checks = (nixpkgs.lib.attrsets.recursiveUpdate self.checks (
          nixpkgs.lib.attrsets.mapAttrs
            (_: pkgs: nixpkgs.lib.attrsets.removeAttrs pkgs [ "freezeFile" ])
            self.packages
        ));
      };
    };
}
