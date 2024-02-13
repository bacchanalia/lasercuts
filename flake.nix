{
  description = "Zoe's Lasercutting Projects";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"  "aarch64-linux"
      "x86_64-darwin" "aarch64-dawrin"
      "x86_64-windows"
    ] (system: with nixpkgs.legacyPackages.${system}; let
      mkApp = name: {
        type = "app";
        program = "${self.packages.${system}.default}/bin/${name}";
      };
    in {
      packages = {
        haskellPackages.lasercutting = haskellPackages.callCabal2nix "lasercutting" ./. {};
        default = self.packages.${system}.haskellPackages.lasercutting;
      };

      apps = lib.flip lib.genAttrs mkApp [
        "compassearrings"
        "compassearrings-v2"
        "pentagramearrings"
      ];

      devShells.default = self.packages.${system}.default.env;
    });
}
