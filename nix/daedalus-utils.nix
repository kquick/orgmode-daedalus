{ mkDerivation, alex-tools, base, bytestring, clock, containers
, deepseq, fetchgit, GraphSCC, lib, monadLib, mtl, panic, pretty
, template-haskell, text
}:
mkDerivation {
  pname = "daedalus-utils";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "04rwdvibphb0zyca3dbx1zmn4r5yg9m48dld4v4bbw78v25nwya5";
    rev = "7cd6f1c0e5a061c158e6b1aaa879fdc530870210";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-utils; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    alex-tools base bytestring clock containers deepseq GraphSCC
    monadLib mtl panic pretty template-haskell text
  ];
  license = "unknown";
}
