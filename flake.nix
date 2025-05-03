{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
        name = "template";
        src = ./.;
      in {
        packages.default = derivation {
          inherit system name src;
          builder = with pkgs; "${bash}/bin/bash";
          args = ["-c" "echo foo > $out"];
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            clang
            clang-tools
            gnumake
            lldb
            hlint

            haskell.compiler.ghc9101
            haskell.packages.ghc9101.haskell-language-server
            cabal-install
            haskellPackages.hoogle
            haskellPackages.ghci-dap
            haskellPackages.haskell-debug-adapter
            haskellPackages.fast-tags
            alex
            happy
          ];
          shellHook = ''
            export SHELL=$(which zsh)
            exec zsh
          '';
        };
      }
    );
}
