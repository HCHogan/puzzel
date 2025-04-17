{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [(import rust-overlay)];
        pkgs = import nixpkgs {
          inherit system overlays;
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
            cmake
            ninja
            gnumake
            lldb

            haskell.compiler.ghc9101
            haskell.packages.ghc9101.haskell-language-server
            cabal-install

            (rust-bin.nightly.latest.default.override
              {
                extensions = [
                  "rust-src"
                  "rust-analyzer"
                  "llvm-tools"
                ];
                # targets = [];
              })
          ];
          shellHook = ''
            export SHELL=$(which zsh)
            exec zsh
          '';
        };
      }
    );
}
