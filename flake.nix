{
  description = "Flakes for running this configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.neovim
            pkgs.ripgrep
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
        packages.treesitter-parsers =
          let
            lib = pkgs.lib;
            parsersAttrSet = pkgs.callPackage ./nix/treesitter-parsers.nix { };
            toNvimPlug = pkgs.callPackage ./nix/set-rtp.nix { };
          in
          toNvimPlug "treesitter-parsers" (parsersAttrSet [ ]);
      });
}
