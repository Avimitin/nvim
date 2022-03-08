# How to install new lsp server

You can use command `:LspInstall <Language>` to install new lspserver.
This command is provided by [nvim-lsp-installer](https://github.com/williamboman/nvim-lsp-installer).

The Lua LSP server will be installed when you using this configuration.
Other LSP server should be installed manually. You can add more automatically installed
server in `lua/plugins/config/lspconfig_cfg.lua` file.

Use vimgrep to find the value `ensure_installed_server` and append your LSP server
name inside the array.

For example, if you want the Golang LSP server always be installed automatically,
you can append `gopls` inside the list.

```lua
local ensure_installed_server = {
  "sumneko_lua",
  "gopls",
}
```

> I know it is hard to find out and setting this variable. I will try to expose it
> in outer file someday.

# Where to rewrite the code action key mappings

To separate the normal key mappings and code key mappings, key mappings are attach
to buffer when launching LSP server. So it might be hard to find but the code
key mappings are set in `lua/plugins/config/lspconfig_cfg.lua` file.

# How to change diagnostic signs

You can find use vimgrep to find value `signs` inside the `lua/plugins/config/lspconfig_cfg.lua`
file.
