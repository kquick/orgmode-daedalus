{ mkDerivation, array, base, data-default, dlist, fetchgit
, hashable, lib, prettyprinter, prettyprinter-ansi-terminal
, unordered-containers, wcwidth
}:
mkDerivation {
  pname = "diagnose";
  version = "2.5.1";
  src = fetchgit {
    url = "https://github.com/mesabloo/diagnose";
    sha256 = "0dzdpqwxhz42l60v3vz3a8d86j3g6grbp45vry13i0b7chrzvbd9";
    rev = "d3ffbb7f376ddc64f3f349fe3a6a8e49d405cde0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base data-default dlist hashable prettyprinter
    prettyprinter-ansi-terminal unordered-containers wcwidth
  ];
  testHaskellDepends = [
    array base data-default dlist hashable prettyprinter
    prettyprinter-ansi-terminal unordered-containers wcwidth
  ];
  homepage = "https://github.com/mesabloo/diagnose#readme";
  description = "Beautiful error reporting done easily";
  license = lib.licenses.bsd3;
}
