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
    sha256 = "04rwdvibphb0zyca3dbx1zmn4r5yg9m48dld4v4bbw78v25nwya5";
    rev = "7cd6f1c0e5a061c158e6b1aaa879fdc530870210";
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
