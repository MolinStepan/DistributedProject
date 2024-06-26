{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-23.11";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system} = {
      default = pkgs.mkShell{
        buildInputs = [
          # pkgs.postgresql_16
          # pkgs.redis
          pkgs.cabal-install
          pkgs.ghc
        ];
      };
    };
  };
}
