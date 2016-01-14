{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // {
    frozendict = callPackage ./pkgs/frozendict { };
  };

  self = rec {
    ycmd = callPackage ./pkgs/ycmd { inherit pythonPackages; };

    rust-nightly = callPackage ./pkgs/rust-nightly { };

    powerline-fonts = callPackage ./pkgs/powerline-fonts { };
  };

in self
