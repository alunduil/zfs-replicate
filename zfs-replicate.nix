let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
          let
            toPackage = file: _: {
              name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
              value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
            };
          in pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            # See https://github.com/Gabrial439/haskell-nix/blob/master/project4/README.md#composing-overrides

          };

        in pkgs.haskellPackages.override {
          overrides = pkgs.lib.composeExtensions generatedOverrides manualOverrides;
        };
      };
    };

    pkgs = import <nixpkgs> { inherit config; };
in { zfs-replicate = pkgs.haskellPackages.zfs-replicate; }
