{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        name = "puzzel";
        src = ./.;
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.default = derivation {
          inherit system name src;
          builder = with pkgs; "${bash}/bin/bash";
          args = ["-c" "cabal build"];
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.ghc9101
            haskell.packages.ghc9101.haskell-language-server
            hlint
            clang
            clang-tools
            cabal-install
            hlint
          ];
          shellHook = ''
            export SHELL=$(which zsh)
            exec zsh
          '';
        };
      }
    );
}
