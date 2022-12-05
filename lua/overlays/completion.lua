local rc = require("overlays.rc")

local repos = {
  -- the completion core
  {
    "hrsh7th/nvim-cmp",
    config = rc.cmp,
    event = "InsertEnter",
    module = "cmp",
    keys = {
      { "n", ":" },
      { "n", "/" },
    },
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

  {
    "hrsh7th/cmp-nvim-lsp-signature-help",
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
    setup = function()
      vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")
    end
  },

  {
    "hrsh7th/cmp-cmdline",
    after = {
      "nvim-cmp",
    },
  },

  {
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    after = "nvim-cmp",
  },
}

return repos
