{ stdenv, fetchurl, buildPythonPackage }:

buildPythonPackage {
  name = "frozendict-0.5";

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/f/frozendict/frozendict-0.5.tar.gz";
    sha256 = "0m4kg6hbadvf99if78nx01q7qnbyhdw3x4znl5dasgciyi54432n";
  };

  meta = {
    homepage = https://github.com/slezica/python-frozendict;
    description = "An immutable dictionary";
    license = stdenv.lib.licenses.mit;
  };
}
