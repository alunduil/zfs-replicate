{ mkDerivation, base, directory, extra, hspec, hspec-discover
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "zfs-replicate";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory extra optparse-applicative
  ];
  testHaskellDepends = [ base hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/zfs-replicate";
  description = "ZFS Snapshot Replicator";
  license = stdenv.lib.licenses.bsd2;
}
