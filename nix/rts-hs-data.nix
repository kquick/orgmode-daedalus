{ mkDerivation, base, bytestring, containers, fetchgit, lib, pretty
, text, vector, vector-builder
}:
mkDerivation {
  pname = "rts-hs-data";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "04rwdvibphb0zyca3dbx1zmn4r5yg9m48dld4v4bbw78v25nwya5";
    rev = "7cd6f1c0e5a061c158e6b1aaa879fdc530870210";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/rts-hs-data; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers pretty text vector vector-builder
  ];
  license = "unknown";
}
