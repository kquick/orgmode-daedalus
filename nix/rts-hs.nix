{ mkDerivation, base, bytestring, containers, fetchgit, filepath
, lib, pretty, rts-hs-data, text, vector, vector-builder
}:
mkDerivation {
  pname = "rts-hs";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/rts-hs; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers filepath pretty rts-hs-data text vector
    vector-builder
  ];
  license = lib.licenses.isc;
}
