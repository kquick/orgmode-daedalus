{ mkDerivation, base, bv-sized, bytestring, containers
, daedalus-utils, daedalus-value, deepseq, fetchgit, GraphSCC, lib
, monadLib, parameterized-utils, rts-hs, rts-hs-data, rts-vm-hs
, template-haskell, text, th-abstraction, transformers, vector
}:
mkDerivation {
  pname = "daedalus-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bv-sized bytestring containers daedalus-utils daedalus-value
    deepseq GraphSCC monadLib parameterized-utils rts-hs rts-hs-data
    rts-vm-hs template-haskell text th-abstraction transformers vector
  ];
  license = "unknown";
}
