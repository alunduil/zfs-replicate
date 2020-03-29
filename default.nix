{ mkDerivation, base, directory, extra, filepath, hspec
, hspec-discover, optparse-applicative, process, QuickCheck, stdenv
, text
}:
mkDerivation {
  pname = "zfs-replicate";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory extra filepath optparse-applicative process text
  ];
  testHaskellDepends = [
    base extra filepath hspec process QuickCheck text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/zfs-replicate";
  description = "ZFS Snapshot Replicator";
  license = stdenv.lib.licenses.bsd2;
}
