{
  description = "sqitch with kronor specific templates";
  nixConfig = {
    extra-substituters = "https://kronor.cachix.org";
    extra-trusted-public-keys = "kronor.cachix.org-1:AcZQBZxsK0SSlBoWmD0dNxfmtSoXueMcDBAhxzGb0MU=";
  };
  inputs = {
    git-hooks.url = "github:cachix/git-hooks.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  };
  outputs =
    { self
    , git-hooks
    , nixpkgs
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
    in
    eachSystem (system:
    let

      sqitchTemplateOverlay = import ./sqitch-template-overlay.nix;

      pkgs = import nixpkgs (
        {
          inherit system;
          overlays = [ sqitchTemplateOverlay ];
        }
      );

      shell = pkgs.mkShell {
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
          };
        };
      };
      packages = {
        default = pkgs.sqitchPg;
      };
      devShells.default = shell;
    }
    );
}
