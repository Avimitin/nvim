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

# Per language configuration snippets

## c/cpp

* Config

C/CPP can be configured to use clangd as default LSP server.

```lua
{ { "c", "cpp" }, "clangd" },
```

* project setup

The clangd respect your cmake settings.
You will need to provide the compile_commands.json file for clangd to identify
your project correctly.

```console
$ cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
$ ln -s Debug/compile_commands.json .
```

* Makefile

If you are writing C and using the Makefile, you can use the `:Dispatch` or `:Make`
command to easily build and debug your code.

* Resources

> * Clangd official site: <https://clangd.llvm.org/>
> * CMake official site: <https://cmake.org/>


## JavaScript/TypeScript

Add the below script into `lua/custom.lua` file.

```lua
langs = {
  "html",
  "css",
  "json",
  { { "javascript", "typescript", "javascriptreact", "typescriptreact" }, "tsserver" },
},
```

Inject eslint into tsserver by enable the null-ls option:

```lua
null_ls = {
  enable_eslint = true, -- require eslint, useful to combine use with tsserver
  enable_prettier = true, -- require prettier, useful when you want format in js/ts{x}
},
```

### Trouble Shooting

* Neovim notify that their is no executable in `$PATH`

Please make sure that you have configured node.js/deno PATH correctly.
If you are using node version manager like `nvm` or other stuff, please
make sure you have enabled correct version before you start the neovim.

## rust

The plugin rust-tools.nvim has already set up LSP, format, and debug utilities.

See <https://github.com/simrat39/rust-tools.nvim/> for what it can do.

> This plugin will setup lspconfig itself, please don't write lspconfig manually.

* Inlay hint

The rust-tools.nvim setting is located in lua/plugins/coding/config/rust-tools.lua.
And inlay hint is set up automatically after you open rust file.

But it will not prompt up by default due to some unknown bug.
It will only show up after you save the buffer.
So you need to manually run command `:w` when you first open the rust code.

* Code action

You can use the keymap <kbd>LEADER ra</kbd> or keymap `gx` set by LSP to view and select
the action for Rust code. Also you can press <kbd>LEADER rr</kbd> to run test or function.

* Code format

You can use `gf` to run the LSP built in format API. It will called the rustfmt
program.

* Debug

You need to install `lldb-vscode`. Then run `:RustDebuggables`, it will open the
debug panel automatically.

* Rust Analyzer Settings

You might want to make some specific rust-analyzer settings for your project.
You can create a file with name `.rust-analyzer.json` in the same directory
with the `Cargo.toml` file. Then put all the configuration you want into it.

Configuration reference: <https://rust-analyzer.github.io/manual.html#configuration>

For example, if you want to enable all feature when editing the code:

```bash
# cd to the root directory of your project
echo '{
  "cargo": {
    "allFeatures": true
  }
}' > .rust-analyzer.json
```

* Gallery

Please read [demo](https://github.com/simrat39/rust-tools.nvim/#demos).
