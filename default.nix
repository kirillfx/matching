{ mkDerivation, aeson, base, binary, bytestring, comonad
, containers, either, free, hspec, lens, mtl, scientific, stdenv
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "matching";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring comonad containers free lens mtl
    scientific time transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson base binary bytestring comonad containers free lens mtl
    scientific time transformers unordered-containers
  ];
  testHaskellDepends = [ base either hspec lens ];
  description = "Order matching algorithm";
  license = stdenv.lib.licenses.unfree;
}
