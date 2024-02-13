with (import <nixpkgs> {});
haskellPackages.callCabal2nix "lasercutting" ./. {}
