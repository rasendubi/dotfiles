{ lib, python3Packages }:
python3Packages.buildPythonApplication {
  name = "naga-1.0";

  src = lib.cleanSource ./.;

  propagatedBuildInputs = [
    python3Packages.evdev
  ];
}
