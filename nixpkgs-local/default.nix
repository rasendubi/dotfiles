{ pkgs ? import <nixpkgs> { } }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    
  };

  self = rec {
    gitbook-cli = (import ./gitbook { inherit pkgs; }).gitbook-cli;
    heroku = (import ./heroku { inherit pkgs; }).heroku-cli;
  };

in self
