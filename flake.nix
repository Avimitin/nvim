{
  description = "Flakes for running this configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    let
      overlay = import ./overlay.nix;
    in
    {
      overlays.default = overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          overlays = [ overlay ];
          inherit system;
        };
        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          settings.verbose = 1;
          programs.nixfmt.enable = true;
        };
      in
      {
        formatter = treefmtEval.config.build.wrapper;
        legacyPackages = pkgs;
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.haskellPackages; [
            pkgs.ghc-for-ts-plugins
            fourmolu
            haskell-language-server
          ];
        };
      }
    );
}
