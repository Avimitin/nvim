{
  pkgs,
  lib,
  neovimConfig,
  extraPlugins ? { },
}:
let
  pluginsMeta = builtins.fromJSON (builtins.readFile ../plugins.json);

  # Fetch plugin using builtins.fetchGit (requires impure evaluation or hashed pinning)
  fetchPluginSrc =
    p:
    let
      isCommit = (p ? version) && (builtins.match "[0-9a-f]{40}" p.version) != null;
      args = {
        url = p.src;
      }
      // (
        if !(p ? version) then
          { }
        else if isCommit then
          { rev = p.version; }
        else
          { ref = p.version; }
      );
    in
    builtins.fetchGit args;

  # Create a derivation for each plugin in the pack/chika/opt directory
  mkPlugin =
    p:
    pkgs.runCommand "vim-plugin-${p.name}" { } ''
      mkdir -p $out/pack/chika/opt/${p.name}
      cp -r ${fetchPluginSrc p}/* $out/pack/chika/opt/${p.name}
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
  packDir = pkgs.symlinkJoin {
    name = "chika-packpath";
    paths = builtins.attrValues finalPlugins;
  };

  # Generate the init.lua
  # We iterate over the names of finalPlugins to ensure we packadd everything that ends up in the set
  pluginNames = builtins.attrNames finalPlugins;

  # Reuse Nixpkgs treesitter grammars
  # We use the nvim-treesitter wrapper to build a plugin with parsers, then extract just the parsers
  # to allow using the nightly nvim-treesitter Lua code from plugins.json.
  ts-grammars = pkgs.vimPlugins.nvim-treesitter.withPlugins (p:
    with p; [
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

  ts-parsers = pkgs.runCommand "treesitter-parsers" { } ''
    mkdir -p $out/parser
    if [ -d ${ts-grammars}/parser ]; then
      ln -s ${ts-grammars}/parser/*.so $out/parser/
    fi
  '';

  initLua = pkgs.writeText "init.lua" ''
    -- Prepend the treesitter parsers (from Nixpkgs)
    vim.opt.rtp:prepend("${ts-parsers}")

    -- Prepend the generated packpath
    vim.opt.packpath:prepend("${packDir}")

    -- Packadd all plugins
    ${lib.concatMapStringsSep "\n" (name: "vim.cmd.packadd('${name}')") pluginNames}

    -- Prepend the configuration source to runtime path
    vim.opt.rtp:prepend("${neovimConfig}")

    -- Setup the configuration
    require("chika").setup()
  '';

in
pkgs.wrapNeovim pkgs.neovim-nightly-bin {
  configure = {
    customRC = "luafile ${initLua}";
  };
  extraMakeWrapperArgs = "--set NEOVIM_EXTERNAL_PLUGIN_MANAGEMENT 1";
}
