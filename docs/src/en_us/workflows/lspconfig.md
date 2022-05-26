# lspconfig

The lspconfig integrate the Microsoft Language Server Protocol into
neovim, so you can have syntax check while coding.

## Files

The lspconfig definition is put into the lua/plugins/load.lua.
The configuration for lspconfig is put into the lua/plugins/config/lspconfig_cfg.lua.

## Keymaps


To separate the normal key mappings and code key mappings, key mappings are attach
to buffer when launching LSP server. Those key mappings are set in
`lua/plugins/config/lspconfig_cfg.lua` file by function `M.lsp_attach`.
You can get the cheatsheets by command: `:nmap g`.

## Configuration

* Lua

The Lua language server is configured to import neovim API by default.

* Diagnostic Signs

The diagnostic text will have nerdfont icon support.

* Lspsaga

I installed the lspsaga plugin to beautify all the LSP code action,
you can configure the display in lua/plugins/config/lspsaga_settings.lua.

## How to install new lsp server

You can use command `:LspInstall <Language>` to install new lspserver.
This command is provided by [nvim-lsp-installer](https://github.com/williamboman/nvim-lsp-installer).

Before installing server, you might need to configured in `lua/custom.lua` file.
See [Customization](../customize.md) for more.
You can check filetype by using command `echo &filetype`.

Default enabled filetype:

```text
bash
c
cpp
go
html
javascript
json
lua
python
sh
toml
```

You can use the `:LspInstallInfo` to use a panel to manage the servers.

The plugin will ensure Lua[^1] language servers is installed.
You can add or delete those ensure installed server in the lua/config/lsp.lua file.
Use the `/` to find the `ensure_installed_server` variable.

```lua
-- add yours or delete in this variable
local ensure_installed_server = {
  "clangd",
  "gopls",
  "sumneko_lua",
}
```

> I know it is hard to find out and setting this variable. I will try to expose it
> in outer file someday.

[^1]: Rust server is also automatically installed and set up by rust-tools.nvim.
For detail please read [Rust.md(WIP)](../plugins/rust.md)

## How to change diagnostic signs

You can find use vimgrep to find value `signs` inside the `lua/plugins/config/lspconfig_cfg.lua`
file.

## Keymappings

| Key           | Function                                                              |
|---------------|-----------------------------------------------------------------------|
| <kbd>gd</kbd> | Open a menu of definition and reference of the hover symbol           |
| <kbd>gp</kbd> | Preview definition of the hover symbol                                |
| <kbd>gh</kbd> | Open document of the hover symbol                                     |
| <kbd>gs</kbd> | Open help page of the current signature                               |
| <kbd>go</kbd> | Open details of the error message                                     |
| <kbd>gj</kbd> | Jump to next error                                                    |
| <kbd>gk</kbd> | Jump to previous error                                                |
| <kbd>ga</kbd> | Open menu of all available code action (Import, Fmt, Swap params)     |
| <kbd>gD</kbd> | Go to declaration (Most LSP server don't support this)                |
| <kbd>gm</kbd> | Go to Type implementation                                             |
| <kbd>gt</kbd> | Go to Type definition                                                 |
| <kbd>gq</kbd> | Open loclist of diagnostic                                            |
| <kbd>gf</kbd> | Run formatter (Require your lsp support, like rust-analyzer/gopls...) |
