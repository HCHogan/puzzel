{
  pkgs,
  lib,
  config,
  ...
}: {
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc9122;
    stack.enable = false;
    cabal = {
      enable = true;
      package = pkgs.cabal-install;
    };
    lsp = {
      enable = true;
      package = pkgs.haskell.packages.ghc9122.haskell-language-server;
    };
  };

  packages = with pkgs; [
    hpack
    ormolu
    hlint
  ];
}

