{ pkgs ? import <nixpkgs> { } }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);

  pythonPackages = pkgs.pythonPackages // rec {
    
  };

  self = rec {
    imapnotify = (import ./imapnotify { inherit pkgs; }).imapnotify;
  };

in self
