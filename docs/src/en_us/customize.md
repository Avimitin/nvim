# Customize

You might not want git to track some frequently changed values.
Like color scheme or some other settings.
Those settings can all define in an optional `Lua` module.

## Details

Rename the `lua/custom.example.lua` file to `lua/custom.lua`, then make your modification.
The `.gitignore` file contains the `custom.lua` file already.

Create a new configuration table and return it at the end.

```lua
-- example
local my_config = {
  theme = "kanagawa",

  langs = {
    "fish",
    "html",
    "json",
    "toml",
    { "go", "gopls" },
    { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "eslint" },
    {
      "python",
      "pyright",
      {
        python = {
          analysis = {
            autoSearchPaths = true,
            diagnosticMode = "workspace",
            useLibraryCodeForTypes = true,
          },
        },
      },
    },
  },

  -- configuration for null-ls lsp injection
  null_ls = {
    enable_stylua_fmt = false, -- require stylua executable
  },

  autocmd_enable = {
    fcitx5 = false, -- require fcitx5-remote
    lastline = true,
    diff_on_commit = false, -- might mess up your window
  },

  markdown = {
    -- must be executable
    preview_browser = "chrome",
  },
}

return my_config -- <- Don't forget to return this table, or the config will not acceive what you configured
```

## Fields

Current supported options:

| option           | meaning                                                       |
|------------------|---------------------------------------------------------------|
| `theme`          | colorscheme, read [colors](./colors.md) for tips and tricks   |
| `autocmd_enable` | List of auto commands that you want to toggle on or off       |
| `langs`          | An array of language layers for nvim-treesitter and lspconfig |
| `markdown`       | Markdown options                                              |

### langs

This fields contains an array of language layer definitions.

* Single string or an array with item
tells the editor to load nvim-treesitter only for this languages.
* An array with two items tells the editor to load both of the nvim-treesitter and lspconfig plugins.
And the second items for the multi-items array should be lsp server that you want to enabled.
* If the array contains three items, the third item will be considered as server configuration and
will be transfer to the lsp server.

* [Available Lsp Servers](https://github.com/williamboman/nvim-lsp-installer#available-lsps)
* [Supported Languages for treesitter](https://github.com/nvim-treesitter/nvim-treesitter#supported-languages)
* [Lsp Server Configuration](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md)

### `autocmd_enable`

| cmd              | function                                                             |
|------------------|----------------------------------------------------------------------|
| `fcitx5`         | Enable fcitx5 auto toggle when switching insert and normal mode      |
| `lastline`       | Enable auto command that jump to last edit line when you open neovim |
| `diff_on_commit` | Enable auto command that open diff window when you commiting         |

