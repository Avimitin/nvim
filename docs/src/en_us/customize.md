# Customize

You might not want git to track some frequently changed values.
Like color scheme or some other settings.
Those settings can all define in an optional `Lua` module.

## Details

Create a file named `custom.lua` under the `~/.config/nvim/lua` directory.
The `.gitignore` file contains the `custom.lua` file already.

Create a new configuration table and return it at the end.

```lua
-- example
local my_config = {
  theme = "kanagawa",

  auto_darkmode = {
    enable = true,
    day_theme = "github_light",
    night_theme = "kanagawa",
    time = {
      begin = "18:30",
      ending = "7:00",
    },
  },

  auto_toggle_fcitx5 = true,

  langs = {
    "bash",
    "fish",
    "html",
    "json",
    "nix",
    "rust",
    { "vim" },
    { "c", "clangd" },
    { "cpp", "clangd" },
    { "go", "gopls" },
    { "javascript", "eslint" },
    { "lua", "sumneko_lua" },
    { "python", "pyright" },
  },

  -- enable vale? Should install vale before setting this to true
  enable_vale = false,
}

return my_config -- <- Don't forget to return this table, or the config will not acceive what you configured
```

## Fields

Current supported options:

| option               | meaning                                                                           |
|----------------------|-----------------------------------------------------------------------------------|
| `theme`              | colorscheme, read [colors](./colors.md) for tips and tricks                       |
| `auto_toggle_fcitx5` | enable this if you want to switch fcitx5 automatically when you leave insert mode |
| `langs`              | An array of language layers for nvim-treesitter and lspconfig                     |
| `enable_vale` | enable vale for markdown and asciidocs |

### langs

This fields contains an array of language layer definitions. Single string or an array with item
tells the editor to load nvim-treesitter only for this languages.
An array with two items tells the editor to load both of the nvim-treesitter and lspconfig plugins.
And the second items for the multi-items array should be lsp server that you want to automatically
installed and enabled.

* [Available Lsp Servers](https://github.com/williamboman/nvim-lsp-installer#available-lsps)
* [Supported Languages for treesitter](https://github.com/nvim-treesitter/nvim-treesitter#supported-languages)
