{ mkDerivation, alex, alex-tools, ansi-terminal, array, base
, bytestring, containers, daedalus-core, daedalus-utils
, daedalus-value, daedalus-vm, directory, fetchgit, file-embed
, filepath, ghc-prim, GraphSCC, happy, lib, monadLib, mtl, panic
, parameterized-utils, pretty, pretty-hex, pretty-show, process
, process-extras, rts-hs, rts-hs-data, simple-get-opt
, template-haskell, text, vector
}:
mkDerivation {
  pname = "daedalus";
  version = "1.0";
  src = fetchgit {
    url = "https://github.com/galoisinc/daedalus";
    sha256 = "15mn9lbalpq57q7mln0815m28lipdvfcnv7hsi4kb61ccj6ybgvx";
    rev = "a4ad7592ef2449fa1da07d2827fc6684293d21ca";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    alex-tools ansi-terminal array base bytestring containers
    daedalus-core daedalus-utils daedalus-value daedalus-vm directory
    filepath ghc-prim GraphSCC monadLib mtl panic parameterized-utils
    pretty pretty-hex rts-hs rts-hs-data template-haskell text vector
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    alex-tools ansi-terminal base bytestring containers daedalus-core
    daedalus-utils daedalus-value daedalus-vm directory file-embed
    filepath pretty-hex pretty-show process process-extras rts-hs
    rts-hs-data simple-get-opt text
  ];
  testHaskellDepends = [
    base bytestring directory filepath process process-extras
    simple-get-opt
  ];
  license = lib.licenses.isc;
}
