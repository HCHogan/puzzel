{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  env.GREET = "devenv";

  packages = [
    pkgs.git
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.packages.ghc9122.ghc;
    stack.enable = false;
    cabal = {
      enable = true;
      package = pkgs.haskell.packages.ghc9122.cabal-install;
    };
    lsp = {
      enable = true;
      package = pkgs.haskell.packages.ghc9122.haskell-language-server;
    };
  };

  # https://devenv.sh/processes/
  # processes.dev.exec = "${lib.getExe pkgs.watchexec} -n -- ls -la";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = ''
    echo hello from $GREET
  '';

  # https://devenv.sh/basics/
  enterShell = ''
    echo "Haskell Environment Loaded."
    echo "GHC: $(ghc --version)"
    echo "HLS: $(haskell-language-server --version)"
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;
}
