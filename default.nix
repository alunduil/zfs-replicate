{ buildPythonApplication, click, glibcLocales, hypothesis
, pylint, pytest, python, stdenv
}:
buildPythonApplication rec {
  pname = "zfs-replicate";
  version = "1.0.0";
  name = "${pname}-${version}";

  src = ./.;

  checkInputs = [
    hypothesis
    pylint
    pytest
  ];

  buildInputs = [
    glibcLocales
  ];

  propagatedBuildInputs = [
    click
  ];

  postPatch = ''
    patchShebangs bin
  '';

  doCheck = true;

  checkPhase = ''
    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"

    pylint zfs/replicate
    pytest
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/alunduil/zfs-replicate;
    description = "ZFS Snapshot Replicator";
    license = licenses.bsd2;
    maintainers = with maintainers; [ alunduil ];
  };
}
