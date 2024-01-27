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
          { name = "bash"; hash = "sha256-nCA8Io6JdAD02jYldGaeciK6s3tOalt3hg1QqRPzYXI="; }
        ];
      });
}
