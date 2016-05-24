{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    
  };

  self = rec {
    rust-nightly = callPackage ./pkgs/rust-nightly {
      date = "2016-03-11";
      # TODO: hash should be different depending on the system
      hash = "0s450rm51z9gywb4vnaradvy23cqyd19yk8j4swrr3v520f4dx6b";
    };
  };

in self
