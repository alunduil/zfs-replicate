let
  pkgs = import <nixpkgs> {};
in
  pkgs.poetry2nix.mkPoetryApplication {
    projectDir = ../.;
  }
