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

  pluginsMeta = builtins.fromJSON (builtins.readFile ../plugins.json);

  # Fetch plugin using fetchgit with rev and sha256
  fetchPluginSrc =
    p:
    pkgs.fetchgit {
      url = p.src;
      rev = p.rev;
      hash = p.hash;
    };

  mkPluginSet =
    ps:
    pkgs.runCommand "my-nvim-plugins-set" { } ''
      mkdir -p $out/pack/core/opt/
      ${lib.concatMapStringsSep "\n" (plugin: ''
        ln -s ${plugin.value} "$out/pack/core/opt/${plugin.name}"
      '') (lib.attrsToList ps)}
    '';

  # Convert the list of plugins from JSON to an attribute set { name = drv; }
  defaultPlugins = builtins.listToAttrs (
    map (p: {
      name = p.name;
      value = fetchPluginSrc p;
    }) pluginsMeta
  );

  # Merge with extraPlugins. extraPlugins takes precedence.
  finalPlugins = defaultPlugins // cfg.overridePlugins;

  # Generate the init.lua
  # We iterate over the names of finalPlugins to ensure we packadd everything that ends up in the set
  pluginNames = builtins.attrNames finalPlugins;

  # Reuse Nixpkgs treesitter grammars
  # We use the nvim-treesitter wrapper to build a plugin with parsers, then extract just the parsers
  # to allow using the nightly nvim-treesitter Lua code from plugins.json.

  # We only use the prebuilt parser
  ts-parsers = pkgs.symlinkJoin {
    name = "my-treesitter-parsers";
    paths =
      (pkgs.vimPlugins.nvim-treesitter.withPlugins (
        p:
        map (
          plugin: if builtins.typeOf plugin == "string" then p.${plugin} else plugin
        ) cfg.treesitter-grammars
      )).dependencies;
  };

  ts-queries = pkgs.runCommand "link-my-treesitter-queries" { } ''
    mkdir -p "$out/queries"
    ${
      cfg.treesitter-grammars
      |> map (plugin: if builtins.typeOf plugin == "string" then plugin else plugin.language)
      |> lib.concatMapStringsSep "\n" (name: ''
        upstream="${finalPlugins.nvim-treesitter}/runtime/queries/${name}"
        if [[ -e "$upstream" ]]; then
          ln -s "$upstream" "$out/queries/${name}"
        fi
      '')
    }
  '';

  # Clean and neovim only file set for downstream
  neovimConfig =
    with pkgs.lib.fileset;
    toSource {
      root = ../.;
      fileset = unions [
        ../indent
        ../lsp
        ../lua
        ../syntax
        ../init.lua
        ../nvim-pack-lock.json
      ];
    };

  initLua = pkgs.writeText "init.lua" ''
    -- Prepend the treesitter parsers
    vim.opt.rtp:prepend("${ts-parsers}")
    -- Prepend the treesitter queries
    vim.opt.rtp:prepend("${ts-queries}")

    -- Prepend the generated packpath
    vim.opt.packpath:prepend("${mkPluginSet finalPlugins}")

    -- Packadd all plugins
    ${lib.concatMapStringsSep "\n" (name: "vim.cmd.packadd('${name}')") pluginNames}

    -- Prepend the configuration source to runtime path
    vim.opt.rtp:prepend("${neovimConfig}")

    -- Setup the configuration
    require("core").setup({use_external_plugins = true})
  '';

  wrappedNvim = pkgs.wrapNeovim cfg.neovim-unwrapped {
    configure = {
      customRC = "luafile ${initLua}";
    };
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
