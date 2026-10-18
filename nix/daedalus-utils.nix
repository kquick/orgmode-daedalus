{ mkDerivation, alex-tools, base, bytestring, clock, containers
, deepseq, fetchgit, GraphSCC, lib, monadLib, mtl, panic, pretty
, template-haskell, text
}:
mkDerivation {
  pname = "daedalus-utils";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "0f3n0zhbp8q63l0b5xbx424m24y3cag3p8p12fc3k119gmcd0k9g";
    rev = "9afef8242f7289971550ab43d472d284b82766b0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-utils; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    alex-tools base bytestring clock containers deepseq GraphSCC
    monadLib mtl panic pretty template-haskell text
  ];
  license = "unknown";
}
