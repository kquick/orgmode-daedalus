{ mkDerivation, base, bytestring, containers, daedalus-utils
, fetchgit, lib, rts-hs, rts-hs-data, text, vector
}:
mkDerivation {
  pname = "daedalus-value";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-value; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers daedalus-utils rts-hs rts-hs-data text
    vector
  ];
  license = "unknown";
}
