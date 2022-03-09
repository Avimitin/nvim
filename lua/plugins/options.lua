if not vim.fn.has("nvim-0.6") then
  -- for filetype.nvim
  -- If using a Neovim version earlier than 0.6.0
  vim.g.did_load_filetypes = 1
end

-- for vsnip
vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")

-- for wildfire
vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }

-- for vim-markdown
vim.g.vim_markdown_conceal_code_blocks = 0
vim.g.vim_markdown_strikethrough = 1
vim.g.vim_markdown_math = 1
