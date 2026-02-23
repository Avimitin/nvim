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
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      flake = {
        overlays = {
          nightly = neovim-nightly-overlay.overlays.default;
        };

        homeModules.nvim = import ./nix/home.nix;
      };

      imports = [
        # Add treefmt flake module to automatically configure and add formatter to this flake
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        { system, inputs', ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              neovim-nightly-overlay.overlays.default
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

          packages = rec {
            neovim-nightly-unwrapped = inputs'.neovim-nightly-overlay.packages.neovim;
            neovim = pkgs.callPackage ./nix/mkNeovim.nix { };
            default = neovim;
          };

          treefmt = {
            projectRootFile = "flake.nix";
            settings.on-unmatched = "debug";
            programs = {
              nixfmt.enable = true;
              stylua.enable = true;
            };
          };
        };
    };
}
