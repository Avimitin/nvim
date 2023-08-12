{
  description = "Flakes for Rust development";

  inputs = {
    # The nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Utility functions
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = import ./overlay.nix;
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.luajitPackages.fennel pkgs.luajitPackages.readline pkgs.fennel-ls pkgs.fnlfmt ];
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
