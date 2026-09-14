{ mkDerivation, alex-tools, base, bytestring, clock, containers
, deepseq, fetchgit, GraphSCC, lib, monadLib, mtl, panic, pretty
, template-haskell, text
}:
mkDerivation {
  pname = "daedalus-utils";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-utils; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    alex-tools base bytestring clock containers deepseq GraphSCC
    monadLib mtl panic pretty template-haskell text
  ];
  license = "unknown";
}
