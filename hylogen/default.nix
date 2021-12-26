{ mkDerivation, base, data-reify, lib, vector-space }:
mkDerivation {
  pname = "hylogen";
  version = "0.1.5.1";
  sha256 = "0ha9bn7mqyqwpxzzcd4p0hm59dbrf3rcnpyihjkgnb7j4wk1f1rx";
  libraryHaskellDepends = [ base data-reify vector-space ];
  homepage = "https://github.com/sleexyz/hylogen";
  description = "GLSL embedded in Haskell";
  license = lib.licenses.mit;
}
