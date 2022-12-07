local nvcfg = vim.g.nvcfg

local repos = {
  {
    "jose-elias-alvarez/null-ls.nvim",
    rc = "null_ls",
    after = "nvim-lspconfig",
    module = "null-ls",
  },

  -- show workspace error,warning,hint... message in a pop up panel
  {
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    cmd = "TroubleToggle",
    config = function()
      require("trouble").setup({})
    end,
  },

  -- deserialize code to generate text object for highlight and other enhancement
  {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    rc = "treesitter",
    ft = nvcfg.treesitter_fts,
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

  {
    "m-demare/hlargs.nvim",
    after = "nvim-treesitter",
    config = function()
      require("hlargs").setup({
        highlight = { link = "Identifier" },
      })
    end,
  },

  -- manage the lsp server
  {
    "neovim/nvim-lspconfig",
    -- it can be load by itself when filetype condition is satisified
    ft = nvcfg.lspconfig_fts,
    rc = "lspconfig",
    -- it can be load by other plugins
    module = "lspconfig",
  },

  -- enhance the lsp UI
  {
    "glepnir/lspsaga.nvim",
    event = "LspAttach",
    rc = "lspsaga",
    cmd = { "Lspsaga" },
  },

  -- Pre-set for rust lsp
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    rc = "rust",
  },

  -- enhance the Cargo dependencies management
  {
    "saecki/crates.nvim",
    rc = "crates",
    opt = true,
  },

  -- debugger plugin
  {
    "mfussenegger/nvim-dap",
    module = "dap",
    rc = "dap",
  },

  -- UI for nvim-dap
  {
    "rcarriga/nvim-dap-ui",
    module = "dapui",
  },

  -- generate quick jump list in side panel
  {
    "simrat39/symbols-outline.nvim",
    rc = "symbols_outline",
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
        sources = {
          ["null-ls"] = {
            ignore = true,
          },
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
