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
  },

  config = function()
    require("completion.rc")
  end,
})
