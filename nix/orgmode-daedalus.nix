{ mkDerivation, base, bytestring, daedalus, diagnose, exceptions
, hspec, HUnit, lib, named-text, optparse-applicative, panic
, parameterized-utils, prettyprinter, recursion-schemes
, rts-hs-data, rts-vm-hs, sayable, tagged, tasty, tasty-ant-xml
, tasty-checklist, tasty-hspec, tasty-hunit, tasty-sugar
, template-haskell, text
}:
mkDerivation {
  pname = "orgmode-daedalus";
  version = "0.1.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring daedalus diagnose named-text panic prettyprinter
    recursion-schemes rts-hs-data rts-vm-hs sayable tagged
    template-haskell text
  ];
  executableHaskellDepends = [
    base named-text optparse-applicative sayable text
  ];
  testHaskellDepends = [
    base exceptions hspec HUnit named-text parameterized-utils
    rts-hs-data rts-vm-hs sayable tasty tasty-ant-xml tasty-checklist
    tasty-hspec tasty-hunit tasty-sugar text
  ];
  description = "Parser/printer for Emacs Org-Mode using Daedalus";
  license = lib.licenses.bsd3;
  mainProgram = "orgmode";
}
