{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    frozendict = callPackage ./pkgs/frozendict { };
  };

  self = rec {
    powerline-fonts = callPackage ./pkgs/powerline-fonts { };
    ycmd = callPackage ./pkgs/ycmd { inherit pythonPackages; };
    rust-nightly = callPackage ./pkgs/rust-nightly { };
  };

in self
