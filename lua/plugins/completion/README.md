# nvim-cmp

The nvim-cmp is a completion plugin.
It will only be loaded when `InsertEnter` event is triggered.
This means that this plugin will be loaded after you enter the insert mode.

## file location

All the nvim-cmp related plugins and itself are placed inside
`lua/plugins/completion/` directory.

## Configuration

The nvim-cmp is configured to use nerdfont to show icons.

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

* [rafamadriz/friendly-snippets](https://github.com/rafamadriz/friendly-snippets)
  > Community contributed coding snippets
* [hrsh7th/nvim-cmp](https://github.com/hrsh7th/nvim-cmp)
  > Core of the completion engine
* [hrsh7th/cmp-path](https://github.com/hrsh7th/cmp-path)
  > Complete the filesystem path
* [hrsh7th/cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp)
  > Complete the LSP symbol
* [hrsh7th/cmp-nvim-lsp-signature-help](https://github.com/hrsh7th/cmp-nvim-lsp-signature-help)
  > Complete the LSP signature
* [hrsh7th/cmp-buffer](https://github.com/hrsh7th/cmp-buffer)
  > Complete the word in current buffer
* [hrsh7th/cmp-vsnip](https://github.com/hrsh7th/cmp-vsnip)
  > Complete with snippets
* [hrsh7th/vim-vsnip](https://github.com/hrsh7th/vim-vsnip)
  > Core of the snippet engine
* [hrsh7th/cmp-cmdline](https://github.com/hrsh7th/cmp-cmdline)
  > Complete the vim command
* [uga-rosa/cmp-dictionary](https://github.com/uga-rosa/cmp-dictionary)
  > Complete vocabulary
* [hrsh7th/cmp-nvim-lsp-document-symbol](https://github.com/hrsh7th/cmp-nvim-lsp-document-symbol)
  > Complete when searching symbols

## Reference

- `:h nvim-cmp`
- `:h vsnip`
