{ mkDerivation, base, containers, stdenv, witherable }:
mkDerivation {
  pname = "partial2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers witherable ];
  homepage = "https://github.com/PaulGustafson/partial";
  description = "A basic partial map typeclass. Instances include Data.Map, List, Maybe a, and the usual partial function type \"a -> Maybe b\".";
  license = stdenv.lib.licenses.mit;
}
