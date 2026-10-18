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
    sha256 = "0f3n0zhbp8q63l0b5xbx424m24y3cag3p8p12fc3k119gmcd0k9g";
    rev = "9afef8242f7289971550ab43d472d284b82766b0";
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
