{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.aarch64-darwin;
  in {
    packages.aarch64-darwin.default = 
      pkgs.haskellPackages.callCabal2nix "effectExperiment" ./. { freer-simple = pkgs.haskellPackages.callPackage ./freer.nix {}; };

    devShells.aarch64-darwin.default = 
      pkgs.haskellPackages.shellFor {
          packages = p: [ self.packages.aarch64-darwin.default ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            git 
          ];
        };

  };
}
