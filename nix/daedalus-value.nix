{ mkDerivation, base, bytestring, containers, daedalus-utils
, fetchgit, lib, rts-hs, rts-hs-data, text, vector
}:
mkDerivation {
  pname = "daedalus-value";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-value; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers daedalus-utils rts-hs rts-hs-data text
    vector
  ];
  license = "unknown";
}
