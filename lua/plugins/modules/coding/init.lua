local config = require("plugins.modules.coding.config")
local use = require("plugins").register

-- preconfigure
config.pre()

local plugs = {
  {
    "jose-elias-alvarez/null-ls.nvim",
    -- enable it when we are writing formal artical
    filetype = { "markdown", "tex", "asciidoc" },
    config = config.null_ls_config,
  },

  -- show workspace error,warning,hint... message in a pop up panel
  {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = config.trouble_nvim_config,
    after = "nvim-lspconfig"
  },

  -- deserialize code to generate text object for highlight and other enhancement
  {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = config.treesitter_config,
    ft = config.treesitter_ft(),
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  },

  -- automatically download and manage lsp server
  {
    "williamboman/nvim-lsp-installer",
    -- setup by lspconfig in lspconfig_cfg.lua file
    module = "nvim-lsp-installer",
  },

  -- manage the lsp server
  {
    "neovim/nvim-lspconfig",
    -- it can be load by itself when filetype condition is satisified
    ft = config.lspconfig_ft(),
    config = config.lspconfig_config,
    -- it can be load by other plugins
    module = "lspconfig",
  },

  -- enhance the lsp UI
  {
    "glepnir/lspsaga.nvim",
    after = "nvim-lspconfig",
    config = config.lspsaga_config,
  },

  -- Pre-set for rust lsp
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    config = config.rust_tools_config,
  },

  -- enhance the Cargo dependencies management
  {
    "saecki/crates.nvim",
    event = {
      "BufRead Cargo.toml",
    },
    requires = {
      {
        "nvim-lua/plenary.nvim",
      },
    },
    config = config.crates_nvim_config,
  },

  -- debugger plugin
  {
    "mfussenegger/nvim-dap",
    module = "dap",
    config = config.dap_config,
  },

  -- UI for nvim-dap
  {
    "rcarriga/nvim-dap-ui",
    module = "dapui",
  },

  -- generate quick jump list in side panel
  {
    "simrat39/symbols-outline.nvim",
    config = config.symbols_outline_config,
    cmd = "SymbolsOutline",
  },

  -- go coding enhancement
  {
    "fatih/vim-go",
    ft = { "go" },
  },

  -- use `gcc` `gbc` to comment
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup({})
    end,
    keys = {
      { "n", "gcc" },
      { "n", "gbc" },
      { "v", "gc" },
      { "v", "gb" },
    },
  },

  -- run command in separate vim/nvim/tmux window
  {
    "tpope/vim-dispatch",
    cmd = "Dispatch",
  },

  -- add a progress bar for lsp server
  {
    "j-hui/fidget.nvim",
    after = "nvim-lspconfig",
    config = function()
      require("fidget").setup({
        text = {
          spinner = "dots",
        },
      })
    end,
  },
}

use(plugs)