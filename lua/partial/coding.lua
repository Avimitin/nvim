return {
  {
    'williamboman/nvim-lsp-installer',
    ft = {
      "bash", "sh",
      "c", "cpp",
      "lua", "go",
      "html",
      "toml",
      "json",
      "python",
      "javascript"
    },
    config = function()
      require("lspconfig")
    end
  },

  {
    'neovim/nvim-lspconfig',
    config = function()
      require("config.lsp")
    end,
    module = "lspconfig"
  },

  {
    'tami5/lspsaga.nvim',
    after = "nvim-lspconfig",
    config = function()
      require("config.lspsaga_setting")
    end
  },

  {
    'simrat39/rust-tools.nvim',
    ft = "rust",
    config = function()
      require("config.rust")
    end
  },

  {
    'saecki/crates.nvim',
    event = {
      "BufRead Cargo.toml"
    },
    requires = {
      {
        'nvim-lua/plenary.nvim'
      }
    },
    config = function()
      require('crates').setup({
        popup = {
          autofocus = true,
          border = "single"
        }
      })
    end
  },
  -- }}}

  {
    'mfussenegger/nvim-dap',
    module = "dap",
    config = function()
      require("config.dap_config")
    end
  },

  {
    'rcarriga/nvim-dap-ui',
    module = "dapui"
  },

  {
    'simrat39/symbols-outline.nvim',
    config = function()
      require("config.symbols")
    end,
    cmd = "SymbolsOutline"
  },

  {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require('config.treesitter')
    end
  },

  {
    'fatih/vim-go',
    config = function()
      require("config.vim-go")
    end,
    ft = {"go"}
  },

  {
    'rhysd/vim-clang-format',
    ft = {
      'cpp', 'c',
      'h', 'hpp'
    }
  },

  -- use `gcc` `gbc` to comment
  {
    'numToStr/Comment.nvim',
    config = function ()
      require("Comment").setup()
    end,
    keys = {
      {'n', 'gcc'},
      {'n', 'gbc'},
      {'v', 'gc'},
      {'v', 'gb'},
    }
  },

  {
    "andrejlevkovitch/vim-lua-format",
    ft = "lua"
  },

  {
    'pechorin/any-jump.vim',
    setup = function()
      vim.g.any_jump_window_width_ratio = 0.8
      vim.g.any_jump_window_height_ratio = 0.9
      vim.g.any_jump_disable_default_keybindings = 1
    end,
    cmd = {
      'AnyJump',
      'AnyJumpBack'
    }
  },

  {
    'tpope/vim-dispatch',
    cmd = "Dispatch",
  },

  {
    'j-hui/fidget.nvim',
    after = "nvim-lspconfig",
    config = function ()
      require'fidget'.setup{}
    end
  }

}
