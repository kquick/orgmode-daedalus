{ mkDerivation, base, bytestring, containers, fetchgit, lib, pretty
, rts-hs-data, text, vector, vector-builder
}:
mkDerivation {
  pname = "rts-vm-hs";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/rts-vm-hs; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers pretty rts-hs-data text vector
    vector-builder
  ];
  license = lib.licenses.isc;
}
