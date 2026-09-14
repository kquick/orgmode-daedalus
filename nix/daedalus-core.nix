{ mkDerivation, base, bv-sized, bytestring, containers
, daedalus-utils, daedalus-value, deepseq, fetchgit, GraphSCC, lib
, monadLib, parameterized-utils, rts-hs, rts-hs-data, rts-vm-hs
, template-haskell, text, th-abstraction, transformers, vector
}:
mkDerivation {
  pname = "daedalus-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
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
