{ mkDerivation, base, bitwise, bytestring, deepseq, fetchgit
, hedgehog, lib, MonadRandom, panic, parameterized-utils, random
, tasty, tasty-hedgehog, th-lift
}:
mkDerivation {
  pname = "bv-sized";
  version = "1.0.8.0.99";
  src = fetchgit {
    url = "https://github.com/GaloisInc/bv-sized";
    sha256 = "1xlkk0whpdg0wxavz6635p958figmk6wirccc4nxai2i9lbryyz3";
    rev = "23243fa0dcb829350e2186cb12c13b3d918892b4";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bitwise bytestring deepseq panic parameterized-utils random
    th-lift
  ];
  testHaskellDepends = [
    base bytestring hedgehog MonadRandom parameterized-utils tasty
    tasty-hedgehog
  ];
  homepage = "https://github.com/GaloisInc/bv-sized";
  description = "a bitvector datatype that is parameterized by the vector width";
  license = lib.licenses.bsd3;
}
