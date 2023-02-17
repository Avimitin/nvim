local register = require("pack").register

register("hrsh7th/nvim-cmp", {
  event = "InsertEnter",
  keys = { ":", "/" },
  dependencies = {
    -- from lsp
    "hrsh7th/cmp-nvim-lsp",
    -- for function parameters
    "hrsh7th/cmp-nvim-lsp-signature-help",
    -- for OS path
    "hrsh7th/cmp-path",
    -- for words in current buffer
    "hrsh7th/cmp-buffer",
    -- for vim command line
    "hrsh7th/cmp-cmdline",

    -- for snippets
    {
      "hrsh7th/vim-vsnip",
      init = function()
        vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")
      end,
    },
    "hrsh7th/cmp-vsnip",
  },

  config = function()
    require("completion.rc")
  end,
})
