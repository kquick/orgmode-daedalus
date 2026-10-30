{ mkDerivation, base, bytestring, containers, daedalus-utils
, fetchgit, lib, rts-hs, rts-hs-data, text, vector
}:
mkDerivation {
  pname = "daedalus-value";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "04rwdvibphb0zyca3dbx1zmn4r5yg9m48dld4v4bbw78v25nwya5";
    rev = "7cd6f1c0e5a061c158e6b1aaa879fdc530870210";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-value; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers daedalus-utils rts-hs rts-hs-data text
    vector
  ];
  license = "unknown";
}
