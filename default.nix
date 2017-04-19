{ mkDerivation, aeson, base, binary, bytestring, comonad
, containers, criterion, deepseq, either, free, hspec, lens
, MonadRandom, mtl, random, scientific, stdenv, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "matching";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring comonad containers deepseq free lens
    MonadRandom mtl scientific time transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson base binary bytestring comonad containers criterion free lens
    MonadRandom mtl random scientific time transformers
    unordered-containers
  ];
  testHaskellDepends = [
    base either hspec lens MonadRandom mtl time transformers
  ];
  description = "Order matching algorithm";
  license = stdenv.lib.licenses.unfree;
}
