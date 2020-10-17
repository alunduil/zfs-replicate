let
  pkgs = import <nixpkgs> {};
  cron-install = import ./default.nix;
in
  pkgs.mkShell {
    buildInputs = [
      cron-install
      pkgs.poetry
      pkgs.python3
      pkgs.python37Packages.virtualenv
    ];
  }
