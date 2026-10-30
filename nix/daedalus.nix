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
    sha256 = "04rwdvibphb0zyca3dbx1zmn4r5yg9m48dld4v4bbw78v25nwya5";
    rev = "7cd6f1c0e5a061c158e6b1aaa879fdc530870210";
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
