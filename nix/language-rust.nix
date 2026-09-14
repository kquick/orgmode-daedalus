{ mkDerivation, aeson, alex, array, base, bytestring, criterion
, deepseq, directory, fetchgit, filepath, happy, HUnit, lib
, prettyprinter, process, statistics, template-haskell
, test-framework, test-framework-hunit, text, time, transformers
, unordered-containers, utf8-string, vector, weigh
}:
mkDerivation {
  pname = "language-rust";
  version = "0.3.0.39";
  src = fetchgit {
    url = "https://github.com/GaloisInc/language-rust";
    sha256 = "0921v2mnwps2f78wnba7bbcnhkfvnfzw455vs8z1n41j8lp1v66c";
    rev = "72b9e6182768d50b491997a1b1e6d683864dbb2e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring deepseq prettyprinter template-haskell
    transformers utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [
    aeson base bytestring directory filepath HUnit prettyprinter
    process test-framework test-framework-hunit text time
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson base bytestring criterion deepseq directory filepath process
    statistics weigh
  ];
  homepage = "https://github.com/harpocrates/language-rust";
  description = "Parsing and pretty printing of Rust code";
  license = lib.licenses.bsd3;
}
