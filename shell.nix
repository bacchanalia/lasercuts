with (import <nixpkgs> {});
(haskellPackages.callPackage (import ./lasercuts.nix) {}).env
