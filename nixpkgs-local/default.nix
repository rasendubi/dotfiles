{ pkgs ? import <nixpkgs> { } }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    
  };

  self = rec {
    heroku = (import ./heroku { inherit pkgs; }).heroku-cli;
  };

in self
