{ mkDerivation, base, bytestring, containers, daedalus-utils
, fetchgit, lib, rts-hs, rts-hs-data, text, vector
}:
mkDerivation {
  pname = "daedalus-value";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "0f3n0zhbp8q63l0b5xbx424m24y3cag3p8p12fc3k119gmcd0k9g";
    rev = "9afef8242f7289971550ab43d472d284b82766b0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-value; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers daedalus-utils rts-hs rts-hs-data text
    vector
  ];
  license = "unknown";
}
