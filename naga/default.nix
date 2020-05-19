{ stdenv, python3Packages }:
python3Packages.buildPythonApplication {
  name = "naga-1.0";

  src = stdenv.lib.cleanSource ./.;

  propagatedBuildInputs = [
    python3Packages.evdev
  ];
}
