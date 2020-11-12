let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./partial2.nix { }