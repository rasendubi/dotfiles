{ pkgs ? import <nixpkgs> { }
}:
with pkgs;
python3Packages.buildPythonApplication {
  name = "naga-1.0";

  src = stdenv.lib.cleanSource ./.;

  propagatedBuildInputs = [
    python3Packages.evdev
  ];
}
