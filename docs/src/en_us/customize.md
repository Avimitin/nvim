# Customize

There are some frequently change value that you don't want to be traked by git.
Like colorscheme or some other settings.
This can all be defined in a optional Lua module.

## Details

Create a file named `custom.lua` under the `~/.config/nvim/lua` directory.
This file is already added into `.gitignore` file, so any changes to it will
not be tracked.

Create a new configuration table and return it at the end.

```lua
local M = {
  theme = "kanagawa",
  has_fcitx5 = true,

  lspconfig = {
    servers = {
      "sumneko_lua",
      "gopls",
      "eslint",
    },
  },

  treesitter = {
    language = {
      "bash",
      "c",
      "nix",
      "rust",
      "toml",
    }
  }
}

return M -- <- Don't forget to return this table, or the config will not acceive what you configured
```

## Fields

Current supported options:

| option                      | meaning                                                                           |
|-----------------------------|-----------------------------------------------------------------------------------|
| `theme`                     | colorscheme, read [colors](./colors.md) for tips and tricks                       |
| `has_fcitx5`                | enable this if you want to switch fcitx5 automatically when you leave insert mode |
| [`lspconfig`](#lspconfig)   | lspconfig specific settings                                                       |
| [`treesitter`](#Treesitter) | treesitter specific settings                                                      |

### lspconfig

See what lsp servers you can use: [Available Lsps](https://github.com/williamboman/nvim-lsp-installer#available-lsps)

| option    | meaning                                                                                                                           |
|-----------|-----------------------------------------------------------------------------------------------------------------------------------|
| `servers` | A list of server name that will be setup connection to the neovim. Key mappings, capabilities... will be attached to those buffer |

### Treesitter

See a list of supported language: [Supported Languages](https://github.com/nvim-treesitter/nvim-treesitter#supported-languages)

| option     | meaning                                                                                                                                |
|------------|----------------------------------------------------------------------------------------------------------------------------------------|
| `language` | A list of file type that will be supported by treesitter. The related parser will be compiled and installed by nvim-treesitter plugins |
