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
        overlay = import ./nix/overlay.nix;
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            fennel-ls               # Fennel LSP Server
            fnlfmt                  # Fennel format
            luajitPackages.fennel   # Compiler
            luajitPackages.readline # Fennel REPL enhanced
            nil                     # Nix LSP Server
            ruby                    # Build script
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
