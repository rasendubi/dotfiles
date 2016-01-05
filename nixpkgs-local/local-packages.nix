{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  self = {
    # Already in master waiting when appear in channel
    xxkb = callPackage ./pkgs/xxkb { };

    # Waiting for https://github.com/NixOS/nixpkgs/issues/11746 appear in channel
    mnemosyne = callPackage ./pkgs/mnemosyne { };

    pythonPackages = pkgs.pythonPackages // {
      frozendict = callPackage ./pkgs/frozendict { };
    };

    ycmd = callPackage ./pkgs/ycmd { };
  };

in self
