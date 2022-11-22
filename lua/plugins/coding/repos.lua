local config = require("plugins.coding.config")
-- preconfigure
config.pre()

local repos = {
  {
    "jose-elias-alvarez/null-ls.nvim",
    config = config.null_ls_config,
    after = "nvim-lspconfig",
    module = "null-ls",
  },

  -- show workspace error,warning,hint... message in a pop up panel
  {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    keys = {
      "n",
      "gq",
    },
  },

  -- deserialize code to generate text object for highlight and other enhancement
  {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = config.treesitter_config,
    ft = config.treesitter_ft,
  },

  {
    "RRethy/vim-illuminate",
    after = "nvim-treesitter",
    config = function()
      require("illuminate").configure({
        -- set highest priority for treesitter, and disable regex search
        providers = { "treesitter", "lsp" },
      })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  },

  {
    "windwp/nvim-ts-autotag",
    after = "nvim-treesitter",
  },

  -- manage the lsp server
  {
    "neovim/nvim-lspconfig",
    -- it can be load by itself when filetype condition is satisified
    ft = config.lspconfig_ft,
    config = config.lspconfig_config,
    -- it can be load by other plugins
    module = "lspconfig",
  },

  -- enhance the lsp UI
  {
    "glepnir/lspsaga.nvim",
    event = "LspAttach",
    config = config.lspsaga_config,
    cmd = { "Lspsaga" },
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
    setup = function()
      vim.api.nvim_create_autocmd("BufRead", {
        group = vim.api.nvim_create_augroup("CmpSourceCargo", { clear = true }),
        pattern = "Cargo.toml",
        callback = function()
          require("cmp").setup.buffer({ sources = { { name = "crates" } } })
        end,
      })
    end,
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

  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()
      require("lsp_lines").toggle()
    end,
    module = "lsp_lines",
  },

  {
    "zbirenbaum/neodim",
    event = "LspAttach",
    config = function()
      require("neodim").setup({
        alpha = 0.7,
        blend_color = "#000000",
        update_in_insert = {
          enable = false,
          delay = 100,
        },
        hide = {
          virtual_text = true,
          signs = true,
          underline = true,
        },
      })
    end,
  },
}

return repos
