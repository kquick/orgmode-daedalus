{ mkDerivation, base, bytestring, containers, fetchgit, lib, pretty
, rts-hs-data, text, vector, vector-builder
}:
mkDerivation {
  pname = "rts-vm-hs";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/rts-vm-hs; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers pretty rts-hs-data text vector
    vector-builder
  ];
  license = lib.licenses.isc;
}
