# nvim-cmp

The nvim-cmp is a completion plugin.
It will only be loaded when `InsertEnter` event is triggered.
This means that this plugin will be loaded after you enter the insert mode.

## file location

The nvim-cmp plugin itself is defined in the lua/partial/cmp.lua file.
And its config is placed in the lua/config/completion.lua file.

## Configuration

The nvim-cmp is configured to use lspkind to show nerdfont icons.

I also enabled the ghost text feature so you can always get inline hint.
For those who don't like this feature, you can close this by updating the
ghost_text variable:

```lua
experimental = {
  ghost_text = false
}
```

## Sources

Below are the available sources:

- cmp-path: a system path completion plugin, use it by inputting the path prefix `./` or `/`
- cmp-buffer: a word completion for the current buffer.
- nvim-lsp: a syntax completion, require LSP is set up.
- cmp-vsnip: a snippet completion. It is already has some basic snippets.
You can define yours in the vsnip/ directory.
- cmp-cmdline: press `/` or `:` key will trigger nvim-cmp to complete neovim buffer and command.

## Reference

- `:h nvim-cmp`
- `:h vsnip`
