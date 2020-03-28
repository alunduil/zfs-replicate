{ mkDerivation, base, directory, extra, hspec, hspec-discover
, optparse-applicative, process, QuickCheck, stdenv, text
}:
mkDerivation {
  pname = "zfs-replicate";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory extra optparse-applicative process text
  ];
  testHaskellDepends = [ base extra hspec process QuickCheck text ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/zfs-replicate";
  description = "ZFS Snapshot Replicator";
  license = stdenv.lib.licenses.bsd2;
}
