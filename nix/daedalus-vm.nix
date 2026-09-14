{ mkDerivation, base, bytestring, containers, daedalus-core
, daedalus-utils, daedalus-value, fetchgit, language-rust, lib
, pretty, rts-vm-hs, template-haskell, text, transformers
}:
mkDerivation {
  pname = "daedalus-vm";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/daedalus-vm; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers daedalus-core daedalus-utils
    daedalus-value language-rust pretty rts-vm-hs template-haskell text
    transformers
  ];
  license = "unknown";
}
