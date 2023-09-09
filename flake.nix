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
        toNvimPlug = name: drvs:
          with builtins;
          let
            paths = map toString drvs;
          in
          pkgs.writeText "set-rtp-for-${name}.lua" ''
            vim.opt.rtp:append("${concatStringsSep "," paths}")
          '';
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.neovim
            pkgs.ripgrep
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
        packages.treesitter-parsers = let
          allTreesitterParsers = pkgs.callPackages ./nix/treesitter-parsers.nix {};
        in
          toNvimPlug "treesitter-parsers" (builtins.attrValues allTreesitterParsers);
      });
}
