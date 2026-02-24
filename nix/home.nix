{ neovim-nightly-overlay, ... }:

{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption mkIf;

  cfg = config.programs.avimitin-nvim;

  wrappedNvim = pkgs.callPackage ./mkNeovim.nix {
    neovim-unwrapped = cfg.neovim-unwrapped;
    extraPlugins = cfg.overridePlugins;
    treesitter-grammars = cfg.treesitter-grammars;
  };
in
{
  imports = [ ];

  options.programs.avimitin-nvim = {
    enable = mkEnableOption "Avimitin's nvim";

    neovim-unwrapped = mkOption {
      type = lib.types.package;
      default = neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.neovim;
      description = "The clean neovim package";
    };

    nvim = mkOption {
      type = lib.types.package;
      default = wrappedNvim;
      description = "The final neovim";
    };

    treesitter-grammars = mkOption {
      type = with lib.types; listOf (either str package);
      default = [
        # "c"
        # "markdown"
        # "markdown_inline"
        # "lua"
        # "query"
        # "vim"
        # "vimdoc"
        # --- above are bundled with neovim ---
      ];
      example = [
        "haskell"
        "pkgs.tree-sitter-grammars.tree-sitter-lean"
      ];
      description = "Configure the bundle treesitter plugins";
    };

    overridePlugins = mkOption {
      type = lib.types.attrs;
      default = { };
      example = {
        "oil.nvim" = "pkgs.fetchFromGitHub { ... };";
      };
      description = "Plugins source to override default bundled plugins";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      cfg.nvim
    ];
  };
}
