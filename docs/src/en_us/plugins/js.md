# JavaScript/TypeScript

Add the below script into `lua/custom.lua` file.

```lua
langs = {
  "html",
  "json",
  { "javascript", "eslint" },
  { "typescript", "eslint" },
},
```

The lsp server client will installed automatically when you
open `.js/.jsx/.ts/.tsx` file.

## Trouble Shooting

* Neovim notify that their is no executable in `$PATH`

Please make sure that you have configured node.js/deno PATH correctly.
If you are using node version manager like `nvm` or other stuff, please
make sure you have enabled correct version before you start the neovim.
