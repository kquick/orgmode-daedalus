{ mkDerivation, alex-tools, base, bytestring, clock, containers
, deepseq, fetchgit, GraphSCC, lib, monadLib, mtl, panic, pretty
, template-haskell, text
}:
mkDerivation {
  pname = "daedalus-utils";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-utils; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    alex-tools base bytestring clock containers deepseq GraphSCC
    monadLib mtl panic pretty template-haskell text
  ];
  license = "unknown";
}
