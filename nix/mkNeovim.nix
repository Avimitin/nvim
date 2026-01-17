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
}:
let
  pluginsMeta = builtins.fromJSON (builtins.readFile ../plugins.json);

  # Fetch plugin using fetchgit with rev and sha256
  fetchPluginSrc =
    p:
    fetchgit {
      url = p.src;
      rev = p.rev;
      sha256 = p.sha256;
    };

  mkPlugin =
    p:
    runCommand "vim-plugin-${p.name}" { } ''
      mkdir -p $out/pack/core/opt/${p.name}
      cp -r ${fetchPluginSrc p}/* $out/pack/core/opt/${p.name}
    '';

  # Convert the list of plugins from JSON to an attribute set { name = drv; }
  defaultPlugins = builtins.listToAttrs (
    map (p: {
      name = p.name;
      value = mkPlugin p;
    }) pluginsMeta
  );

  # Merge with extraPlugins. extraPlugins takes precedence.
  finalPlugins = defaultPlugins // extraPlugins;

  # Combine into a single packpath directory
  packDir = symlinkJoin {
    name = "core-packpath";
    paths = builtins.attrValues finalPlugins;
  };

  # Generate the init.lua
  # We iterate over the names of finalPlugins to ensure we packadd everything that ends up in the set
  pluginNames = builtins.attrNames finalPlugins;

  # Reuse Nixpkgs treesitter grammars
  # We use the nvim-treesitter wrapper to build a plugin with parsers, then extract just the parsers
  # to allow using the nightly nvim-treesitter Lua code from plugins.json.
  ts-grammars = vimPlugins.nvim-treesitter.withPlugins (
    p: with p; [
      bash
      c
      cpp
      css
      comment
      diff
      gitcommit
      haskell
      javascript
      typescript
      tsx
      typst
      llvm
      lua
      ocaml
      ocaml_interface
      regex
      ruby
      python
      rust
      proto
      scala
      nix
      vimdoc
      query
      markdown
      markdown_inline
      yaml
      zig
      meson
    ]
  );

  ts-parsers = runCommand "treesitter-parsers" { } ''
    mkdir -p $out/parser
    if [ -d ${ts-grammars}/parser ]; then
      ln -s ${ts-grammars}/parser/*.so $out/parser/
    fi
  '';

  # Clean and neovim only file set for downstream
  neovimConfig =
    with lib.fileset;
    toSource {
      root = ../.;
      fileset = unions [
        ../after
        ../ftdetect
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

    -- Prepend the generated packpath
    vim.opt.packpath:prepend("${packDir}")

    -- Packadd all plugins
    ${lib.concatMapStringsSep "\n" (name: "vim.cmd.packadd('${name}')") pluginNames}

    -- Prepend the configuration source to runtime path
    vim.opt.rtp:prepend("${neovimConfig}")

    -- Setup the configuration
    require("core").setup()
  '';

in
wrapNeovim neovim-unwrapped {
  configure = {
    customRC = "luafile ${initLua}";
  };
  extraMakeWrapperArgs = "--set NEOVIM_EXTERNAL_PLUGIN_MANAGEMENT 1";
}
