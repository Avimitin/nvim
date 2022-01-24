return {
  {
    'rafamadriz/friendly-snippets',
    event = "InsertEnter"
  },

  {
    'hrsh7th/nvim-cmp',
    after = "friendly-snippets",
    config = function()
      require("config.completion")
    end,
    requires = {
      'onsails/lspkind-nvim'
    }
  },

  {
    'hrsh7th/cmp-path',
    after = {
      'nvim-cmp'
    }
  },

  {
    'hrsh7th/cmp-nvim-lsp',
    after = {
      'nvim-cmp'
    }
  },

  {
    'hrsh7th/cmp-buffer',
    after = {
      'nvim-cmp'
    }
  },

  {
    'hrsh7th/cmp-vsnip',
    after = {
      'nvim-cmp'
    }
  },

  {
    'hrsh7th/vim-vsnip',
    after = {
      'nvim-cmp'
    }
  },
}
