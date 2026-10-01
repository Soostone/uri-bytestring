{
  inputs = {
    # Stable release branch; the exact rev is pinned in flake.lock.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    haskell-flake.url = "github:srid/haskell-flake/1.0.0";
  };

  outputs = inputs @ { self, ... }: let
    # The systems flake-utils' eachDefaultSystem would use, inlined so we can
    # stay pure (no builtins.currentSystem) and avoid a flake-utils input.
    # Nix picks the right one at selection time (e.g. `nix develop .`).
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

    forSystem = system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        # Local packages are auto-discovered from the top-level .cabal file.
        haskellFlakesLib = (inputs.haskell-flake.lib) { inherit pkgs; };

        project = haskellFlakesLib.evalHaskellProject {
          projectRoot = ./.;
          modules = [{
            # Scope the dev shell to what this repo needs on top of ghc:
            # cabal + hlint (hoogle comes with the shell by default).
            defaults.devShell.tools = hp: with hp; { inherit cabal-install hlint; };
          }];
        };
      in {
        package = project.packages.uri-bytestring.package;
        devShell = project.devShell;
        checks = project.checks;
      };

    # Build an attrset keyed by system from a per-system selector. Values stay
    # lazy, so evaluating the flake only instantiates what you select.
    perSystem = sel: builtins.listToAttrs (
      map (system: { name = system; value = sel (forSystem system); }) systems
    );
  in {
    packages = perSystem (fs: { default = fs.package; });
    devShells = perSystem (fs: { default = fs.devShell; });
    # The package derivation runs the full tasty suite in its check phase;
    # expose it here so `nix build .#checks.<system>` / `nix flake check` run it.
    checks = perSystem (fs: fs.checks // { uri-bytestring-test = fs.package; });
  };
}
