{ mkDerivation, base, bytestring, deepseq, fetchgit, lib
, template-haskell, text
}:
mkDerivation {
  pname = "alex-tools";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/GaloisInc/alex-tools";
    sha256 = "1625imhzznbkbsidv7djg0h551fxs79s2q9irif3bh6a4as2g92j";
    rev = "332ab046786cc3a1ff9e320a1df388de71070131";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring deepseq template-haskell text
  ];
  description = "A set of functions for a common use case of Alex";
  license = lib.licenses.isc;
}
