{
  description = "Nvim Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      neovim-nightly-overlay,
      ...
    }:
    let
      overlay = import ./overlay.nix;
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      flake = {
        overlays = {
          default = overlay;
          nightly = neovim-nightly-overlay.overlays.default;
        };

        # Clean and neovim only file set for downstream
        neovimConfig =
          let
            lib = import "${nixpkgs.outPath}/lib";
          in
          with lib.fileset;
          toSource {
            root = ./.;
            fileset = unions [
              ./after
              ./ftdetect
              ./indent
              ./lsp
              ./lua
              ./syntax
              ./init.lua
              ./plugins.json
            ];
          };
      };

      imports = [
        # Add treefmt flake module to automatically configure and add formatter to this flake
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        { system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              neovim-nightly-overlay.overlays.default
              overlay
            ];
          };
        in
        {
          # Override the default "pkgs" attribute in per-system config.
          _module.args.pkgs = pkgs;

          # Although the pkgs attribute is already override, but I am afraid
          # that the magical evaluation of "pkgs" is confusing, and will lead
          # to debug hell. So here we use the "pkgs" in "let-in binding" to
          # explicitly told every user we are using an overlayed version of
          # nixpkgs.
          legacyPackages = pkgs;

          packages = {
            default = pkgs.callPackage ./nix/mkNeovim.nix {
              inherit (self) neovimConfig;
            };
            neovim = pkgs.callPackage ./nix/mkNeovim.nix {
              inherit (self) neovimConfig;
            };
          };

          devShells = {
            default = pkgs.mkShell {
              buildInputs = [
                pkgs.ghc-for-ts-plugins
                pkgs.fourmolu
                pkgs.haskell-language-server
              ];
            };
          };

          treefmt = {
            projectRootFile = "flake.nix";
            settings.on-unmatched = "debug";
            programs = {
              nixfmt.enable = true;
            };
          };
        };
    };
}
