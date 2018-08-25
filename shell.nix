let
  dependencies = ps: [
    ps.click
    ps.hypothesis
    ps.pylint
    ps.pytest
  ];

  pkgs = import <nixpkgs> { };
in
  (pkgs.python36.withPackages dependencies).env
