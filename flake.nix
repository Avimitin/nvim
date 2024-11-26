{
  description = "Flakes for running this configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = import ./overlay.nix;
    in
    { overlays.default = overlay; }
    //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { overlays = [ overlay ]; inherit system; };
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        legacyPackages = pkgs;
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.haskellPackages; [
            pkgs.ghc-for-ts-plugins
            fourmolu
            haskell-language-server
          ];
        };
      });
}
