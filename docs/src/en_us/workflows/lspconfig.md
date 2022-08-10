# lspconfig

The lspconfig integrate the Microsoft Language Server Protocol into
neovim, so you can have syntax check while coding.

## Files

The configuration for lspconfig is put into the lua/plugins/coding/config/lspconfig.lua.

## Keymaps

To separate the normal key mappings and code key mappings, key mappings are attach
to buffer when launching LSP server. Those key mappings are set in
`lua/plugins/coding/keymap.lua` file by function `M.lsp_attach`.

| keys              | map                                                      |
|-------------------|----------------------------------------------------------|
| <kbd>gd</kbd>     | Open finder for symbol under cursor                      |
| <kbd>gp</kbd>     | Preview definition for symbol under cursor               |
| <kbd>gh</kbd>     | Open document for symbol under cursor                    |
| <kbd>Ctrl u</kbd> | Scoll up document                                        |
| <kbd>Ctrl d</kbd> | Scroll down document                                     |
| <kbd>gs</kbd>     | Get help for symbol under cursor                         |
| <kbd>go</kbd>     | Show diagnostic for current line                         |
| <kbd>gj</kbd>     | Jump to next error                                       |
| <kbd>gk</kbd>     | Jump to previous error                                   |
| <kbd>gr</kbd>     | Rename symbol under cursor                               |
| <kbd>ga</kbd>     | Open code action for symbol under cursor                 |
| <kbd>gD</kbd>     | Jump to declaration (Most of the LSP don't support this) |
| <kbd>gm</kbd>     | Jump to implementation                                   |
| <kbd>gt</kbd>     | Jump to type definition                                  |
| <kbd>gq</kbd>     | Open a panel that contains all the diagnostic in project |
| <kbd>gf</kbd>     | Format code in the buffer                                |
| <kbd>gl</kbd>     | Switch diagnostic hint between line mode and inline mode |

## How to install new lsp server

Modify the `lua/custom.lua` file. Add a pair inside the `langs` field in this form:
`{ "filetype", "server name" }`. You can check filetype by using command `echo &filetype`.

- Example:

```lua
-- file: lua/custom.lua
local custom = {
  langs = {
    { "c", "clangd" },
    { "cpp", "clangd" },
    { "go", "gopls" },
    { "javascript", "eslint" },
    { "typescript", "eslint" },
    { "python", "pyright" },
  }
}

return custom
```

Install the server by your system package manager, and add the executable name
into the custom.lua file.

> * Rust server is automatically installed and set up by rust-tools.nvim.
> Don't add the rust-analyzer executable here.
> For detail please read [Rust.md(WIP)](../plugins/rust.md)
> * Lua server is installed and pre-configured by default, you don't need to configured it too.

## How to change diagnostic signs

You can find use vimgrep to find value `signs` inside the `lua/plugins/coding/config/lspconfig_cfg.lua`
file.
