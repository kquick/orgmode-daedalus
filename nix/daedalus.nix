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
    url = "https://github.com/GaloisInc/daedalus";
    sha256 = "0wi31zy84gl5v8fjinhmlwf9gsnbkzdsij9lvns15v8b5jds5xsk";
    rev = "443c848a62a90a57cb7b8a02e8b2a1b83581affc";
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
