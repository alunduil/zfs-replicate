let
  pkgs = import <nixpkgs> {};
  zfs-replicate = import ./default.nix;
in
  pkgs.mkShell {
    inputsFrom = [zfs-replicate];
    buildInputs = [
      pkgs.poetry
      pkgs.python38Packages.pre-commit
      pkgs.python38Packages.virtualenv
      zfs-replicate
    ];
    shellHook = ''
      unset SOURCE_DATE_EPOCH
    '';
  }
