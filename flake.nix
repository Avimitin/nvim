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
        packages.test-ts-parser = pkgs.generate-nvim-treesitter-parsers [
          { name = "bash"; hash = "sha256-b1r/T+Y4Kmui/pHsncozP8OO6rMMHJj+Xaa2Qzwo/cI="; }
        ];
      });
}
