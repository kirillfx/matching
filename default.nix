{ mkDerivation, aeson, base, binary, bytestring, comonad
, containers, free, mtl, scientific, stdenv, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "matching";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring comonad containers free mtl scientific
    time transformers unordered-containers
  ];
  description = "Order matching algorithm";
  license = stdenv.lib.licenses.unfree;
}
