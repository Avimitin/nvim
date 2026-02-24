{
  wrapNeovim,
  neovim-unwrapped,
  vimPlugins,
  symlinkJoin,
  writeText,
  runCommand,
  lib,
  fetchgit,
  extraPlugins ? { },
  treesitter-grammars ? [
    "bash"
    "nix"
    "javascript"
  ],
}:
let
  pluginsMeta = builtins.fromJSON (builtins.readFile ../plugins.json);

  # Fetch plugin using fetchgit with rev and sha256
  fetchPluginSrc =
    p:
    fetchgit {
      url = p.src;
      rev = p.rev;
      hash = p.hash;
    };

  mkPluginSet =
    ps:
    runCommand "my-nvim-plugins-set" { } ''
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
  finalPlugins = defaultPlugins // extraPlugins;

  # Generate the init.lua
  # We iterate over the names of finalPlugins to ensure we packadd everything that ends up in the set
  pluginNames = builtins.attrNames finalPlugins;

  # We only use the prebuilt parser
  ts-parsers = symlinkJoin {
    name = "my-treesitter-parsers";
    paths =
      (vimPlugins.nvim-treesitter.withPlugins (
        p:
        map (
          plugin: if builtins.typeOf plugin == "string" then p.${plugin} else plugin
        ) treesitter-grammars
      )).dependencies;
  };

  ts-queries = runCommand "link-my-treesitter-queries" { } ''
    mkdir -p "$out/queries"
    ${
      treesitter-grammars
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
    with lib.fileset;
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

  initLua = writeText "init.lua" ''
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

in
wrapNeovim neovim-unwrapped {
  configure = {
    customRC = "luafile ${initLua}";
  };
}
