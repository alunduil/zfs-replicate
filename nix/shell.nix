let
  pkgs = import <nixpkgs> {};
  zfs-replicate = import ./default.nix;
in
  pkgs.mkShell {
    inputsFrom = [zfs-replicate];
    buildInputs = [
      zfs-replicate
      pkgs.poetry
      pkgs.pre-commit
      pkgs.python38Packages.virtualenv
    ];
    shellHook = ''
      unset SOURCE_DATE_EPOCH
    '';
  }
