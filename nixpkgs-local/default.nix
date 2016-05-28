{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    
  };

  self = rec {
    rust-nightly = callPackage ./pkgs/rust-nightly {
      date = "2016-05-28";
      # TODO: hash should be different depending on the system
      hash = "0f9rx672v97f5bn6mnb1dgyczyf5f8vcjp55yvasflvln1w64krv";
    };
  };

in self
