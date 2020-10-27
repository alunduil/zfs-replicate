let
  pkgs = import <nixpkgs> {};
  zfs-replicate = import ./default.nix;
in
  pkgs.mkShell {
    buildInputs = [
      zfs-replicate
      (pkgs.poetry2nix.mkPoetryEnv { projectDir = ../.; })
    ];
    shellHook = ''
      unset SOURCE_DATE_EPOCH
    '';
  }
