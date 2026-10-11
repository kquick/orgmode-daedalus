{ mkDerivation, base, bytestring, containers, daedalus-core
, daedalus-utils, daedalus-value, fetchgit, language-rust, lib
, pretty, rts-vm-hs, template-haskell, text, transformers
}:
mkDerivation {
  pname = "daedalus-vm";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
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
