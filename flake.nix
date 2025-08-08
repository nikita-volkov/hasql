{
  description = "hasql";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forallSystems =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          })
        );
      nixpkgsFor =
        system:
        import nixpkgs {
          inherit system;
          config = { };
        };
      hpkgsFor =
        system: pkgs:
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc910.override {
          overrides = self: super: {
            rel8 = dontCheck super.rel8; # requires tmp-postgres
            hlint_3_10 = super.hlint_3_10.override rec {
              ghc-lib-parser = super.ghc-lib-parser_9_12_2_20250421;
              ghc-lib-parser-ex = super.ghc-lib-parser-ex_9_12_0_0.override {
                inherit ghc-lib-parser;

              };
            };
          };
        };
    in
    {
      packages = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          hasql = haskellPackages.callCabal2nix "hasql" ./. { };
          default = self.packages.${system}.hasql;
        }
      );
      devShells = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          hasql = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.hasql ];
            buildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
            ];
          };
          default = self.devShells.${system}.hasql;
        }
      );
    };
}
