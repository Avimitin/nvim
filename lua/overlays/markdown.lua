vim.g.mkdp_browser = vim.g.nvcfg.markdown.previewer

vim.g.mkdp_open_to_the_world = 1
vim.g.mkdp_port = "57843"
return {
  -- markdown toc
  {
    "mzlogin/vim-markdown-toc",
    cmd = {
      "GenTocGFM",
    },
  },

  -- markdown preview
  {
    "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
    ft = {
      "markdown",
    },
  },

  -- table editing enhancement
  {
    "dhruvasagar/vim-table-mode",
    cmd = "TableModeToggle",
  },
}
