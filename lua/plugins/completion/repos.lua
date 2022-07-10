local config = require("plugins.completion.config")

local repos = {
  -- lot's of pre-set snippets
  {
    "rafamadriz/friendly-snippets",
    event = "InsertEnter",
    keys = {
      { "n", ":" },
      { "n", "/" },
    },
  },

  -- the completion core
  {
    "hrsh7th/nvim-cmp",
    after = "friendly-snippets",
    config = config.nvim_cmp_config,
  },

  -- completion source for system path
  {
    "hrsh7th/cmp-path",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for lspconfig
  {
    "hrsh7th/cmp-nvim-lsp",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for word in current buffer
  {
    "hrsh7th/cmp-buffer",
    after = {
      "nvim-cmp",
    },
  },

  -- completion source for vsnip snippet plugin
  {
    "hrsh7th/cmp-vsnip",
    after = {
      "nvim-cmp",
    },
  },

  -- the snippet core
  {
    "hrsh7th/vim-vsnip",
    after = {
      "nvim-cmp",
    },
  },

  {
    "hrsh7th/cmp-cmdline",
    after = {
      "nvim-cmp",
    },
  },

  {
    "uga-rosa/cmp-dictionary",
    after = "nvim-cmp",
    config = config.cmp_dictionary_config,
  },
}

return repos
