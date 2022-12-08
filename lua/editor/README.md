# Docs

This folder contains some basic setup for neovim itself.

## init.lua

This file is the entry point of the whole configuration. It will initialize default configuration
for this configuration, read and parse configuration, and finally load all the plugins.

You can edit configuration in `~/.config/nvim/init.lua` file. You can also create `.neovim.lua` file
in your project directory to have different configuration in different project.

For example, the javascript and javascriptreact are configured to use tsserver and eslint.
And you have two project maintaining simultaneously, one is a React frontend project and the another
one is a backend driven by Deno. It will be unconvenient to always change settings in the nvim config
directory.

In this scenario, you can create a `.neovim.lua` file with the below content in your Deno project:

```lua
-- .neovim.lua

return {
  coding = {
    langs = {
      { "javascript", "denols" }
    }
  }
}
```

Then neovim will use `tsserver` for you React project, but use `denols` for your deno project.

Another common case is Rust project with different features. You might feel annoying to turn
on and off features for different Rust project, because that require you to always cd into
neovim directory and modify the global configuration.

With `.neovim.lua` file, you can have per-project editor configuration:

```bash
cargo new my-new-project
cd my-new-project
tee -a .neovim.lua <<END
return {
  coding = {
    rust = {
      cargo = {
        allFeatures = true
      }
    }
  }
}
END >
```

## keymap.lua

This file contains basic key remapping. It only contains essential key mappings. Plugins key mapping
are defined in `lua/overlays/rc/_setups.lua`.

## options.lua

This file set up some useful option for neovim, for example, enable the number column.
I've added comments for the effect of those options, if you got some unexpected behavior, feel
free to close them.
