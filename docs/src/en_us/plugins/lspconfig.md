# lspconfig

The lspconfig integrate the Microsoft Language Server Protocol into
neovim, so you can have syntax check while coding.

## Files

The lspconfig definition is put into the lua/partial/coding.lua.
The configuration for lspconfig is put into the lua/config/lsp.lua.

## Servers

I have add `lspinstall` plugin to help simplify the language server installation.
You can use `:LspInstall` command to install the server you want.
You can also use the `:LspInstallInfo` to use a panel to manage the servers.

The plugin will ensure CPP/Lua/Golang/Rust[^1] lanaguge servers are installed.
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

[^1]: Rust server is set up by rust-tools.nvim. For detail please read
[Rust.md(WIP)](./.)

## Keymaps

Keymaps are defined in lua/utils.lua by function `M.lsp_attach`.
You can get the cheatsheets by command: `:nmap g`.

## Configuration

* Lua

The Lua language server is configured to import neovim API by default.

* Diagnostic Signs

The diagnostic text will have nerdfont icon support.

* Lspsaga

I installed the lspsaga plugin to beautify all the LSP code action,
you can configure the display in lua/config/lspsaga_settings.lua.
