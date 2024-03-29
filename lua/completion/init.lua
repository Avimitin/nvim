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
    -- for vim command line
    "hrsh7th/cmp-cmdline",

    -- for snippets
    {
      "hrsh7th/vim-vsnip",
      init = function()
        vim.g.vsnip_snippet_dir = vim.fn.stdpath("config") .. "/vsnip"
      end,
    },
    "hrsh7th/cmp-vsnip",
  },

  config = function()
    require("completion.rc")
  end,
})
