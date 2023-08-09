return {
  -- Deep dark purple colorscheme
  {
    "rebelot/kanagawa.nvim",
    config = function()
      require("plugins.ui.kanagawa")
    end,
  },

  --- List of nerd-font icons
  {
    "kyazdani42/nvim-web-devicons",
    lazy = true,
    config = function()
      require("nvim-web-devicons").setup({
        override = {
          ml = {
            icon = "",
            color = "#e37933",
            cterm_color = "166",
            name = "Ml",
          },
        },
      })
    end,
  },

  -- Status line
  {
    "glepnir/galaxyline.nvim",
    event = "UIEnter",
    config = function()
      require("plugins.ui.statusline")
    end,
  },

  -- tab line
  {
    "akinsho/nvim-bufferline.lua",
    event = "BufRead",
    config = function()
      require("plugins.ui.bufferline")
    end,
  },

  -- Indent guide line
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufRead",
    config = function()
      require("plugins.ui.indent")
    end,
  },

  -- Notification UI
  {
    "rcarriga/nvim-notify",
    event = "UIEnter",
    config = function()
      vim.notify = require("notify")
    end,
  },

  -- Scrollbar UI
  {
    "petertriho/nvim-scrollbar",
    lazy = true,
    event = "BufReadPost",
    config = function()
      require("scrollbar").setup({
        marks = {
          Error = { text = { "" } },
          Warn = { text = { "" } },
          Hint = { text = { "" } },
          Info = { text = { "" } },
          GitAdd = { text = "▕" },
          GitChange = { text = "▕" },
        },
        excluded_buftypes = {
          "terminal",
        },
        excluded_filetypes = {
          "prompt",
          "TelescopePrompt",
          "noice",
          "Git",
          "cmp_menu",
          "cmp_docs",
        },
        handlers = {
          cursor = false,
        },
      })
    end,
  },

  -- Dim the inactive variable/function
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

  -- Display diagnostic inline
  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    lazy = true,
    config = function()
      require("lsp_lines").setup()
      require("lsp_lines").toggle()
    end,
  },

  {
    "folke/todo-comments.nvim",
    event = "LspAttach",
    config = function()
      require("todo-comments").setup({
        signs = false,
      })
    end,
  },

  -- prettify the input and select ui
  {
    "stevearc/dressing.nvim",
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.input(...)
      end
    end,
  },

  {
    "folke/noice.nvim",
    event = "VeryLazy",
    config = function()
      require("noice").setup({
        lsp = {
          -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
          override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
          },
        },
        -- you can enable a preset for easier configuration
        presets = {
          bottom_search = true, -- use a classic bottom cmdline for search
          command_palette = true, -- position the cmdline and popupmenu together
          long_message_to_split = true, -- long messages will be sent to a split
          inc_rename = false, -- enables an input dialog for inc-rename.nvim
          lsp_doc_border = false, -- add a border to hover docs and signature help
        },
      })
    end,
  },

  {
    "rainbowhxch/beacon.nvim",
    event = "VeryLazy",
  },
}
