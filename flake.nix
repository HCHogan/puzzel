{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
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
        name = "puzzel";
        src = ./.;
      in {
        packages.default = derivation {
          inherit system name src;
          builder = with pkgs; "${bash}/bin/bash";
          args = ["-c" "echo foo > $out"];
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gnumake
            llvmPackages_16.libllvm
            llvmPackages_16.bintools
            llvmPackages_16.libcxxClang

            haskell.compiler.ghc912
            haskell.packages.ghc912.haskell-language-server
            haskellPackages.hoogle
            haskellPackages.ghci-dap
            haskellPackages.haskell-debug-adapter
            haskellPackages.fast-tags
            haskellPackages.alex
            haskellPackages.happy
            haskellPackages.cabal-fmt
            haskellPackages.fourmolu
            cabal-install
            hlint
          ];
        };
      }
    );
}
