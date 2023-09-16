{ mkDerivation, ansi-terminal, base, bytestring, directory, HaTeX
, hxt, lib, optparse-applicative, parsec, tagsoup, text
}:
mkDerivation {
  pname = "l2b2l";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring directory HaTeX hxt
    optparse-applicative parsec tagsoup text
  ];
  executableHaskellDepends = [
    ansi-terminal base bytestring directory HaTeX hxt
    optparse-applicative parsec tagsoup text
  ];
  testHaskellDepends = [
    ansi-terminal base bytestring directory HaTeX hxt
    optparse-applicative parsec tagsoup text
  ];
  license = lib.licenses.gpl3Only;
}
