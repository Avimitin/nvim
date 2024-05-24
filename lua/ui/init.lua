local register = require("pack").register

-- Deep dark purple colorscheme
register("rebelot/kanagawa.nvim", {
  lazy = true,
  config = function()
    require("ui.kanagawa")
  end,
})

--- List of nerd-font icons
register("kyazdani42/nvim-web-devicons", {
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
      override_by_extension = {
        ["sc"] = {
          icon = "",
          color = "#cc3e44",
          cterm_color = "167",
          name = "Scala",
        },
      },
    })
  end,
})

-- Status line
register("Avimitin/galaxyline.nvim", {
  branch = "global-status-line",
  event = "UIEnter",
  config = function()
    require("ui.statusline")
  end,
})

-- Indent guide line
register("lukas-reineke/indent-blankline.nvim", {
  event = "BufRead",
  config = function()
    require("ui.indent")
  end,
})

-- Notification UI
register("rcarriga/nvim-notify", {
  event = "UIEnter",
  config = function()
    require("notify").setup({
      timeout = 2000,
      top_down = false,
    })
    vim.notify = require("notify")
  end,
})

-- Scrollbar UI
register("petertriho/nvim-scrollbar", {
  lazy = true,
  event = "BufReadPost",
  config = function()
    require("scrollbar").setup({
      marks = {
        Search = { text = { "" } },
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
        "markdown",
      },
      handlers = {
        cursor = false,
        diagnostic = false,
      },
    })
  end,
})

-- prettify the input and select ui
register("stevearc/dressing.nvim", {
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
})

register("folke/noice.nvim", {
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
        bottom_search = false, -- use a classic bottom cmdline for search
        command_palette = true, -- position the cmdline and popupmenu together
        long_message_to_split = true, -- long messages will be sent to a split
        inc_rename = false, -- enables an input dialog for inc-rename.nvim
        lsp_doc_border = true, -- add a border to hover docs and signature help
      },
      views = {
        cmdline_popup = {
          position = {
            row = "50%",
            col = "50%",
          },
          border = {
            style = "none",
            padding = { 1, 2 },
          },
          filter_options = {},
          win_options = {
            winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
          },
        },
      },
      routes = {
        {
          filter = {
            event = "msg_show",
            kind = "",
          },
          opts = { skip = true },
        },
      },
    })
  end,
})

register("luukvbaal/statuscol.nvim", {
  event = "VeryLazy",
  config = function()
    local builtin = require("statuscol.builtin")
    require("statuscol").setup({
      relculright = true,
      setopt = true,
      segments = {
        { text = { " " } },
        { text = { builtin.foldfunc }, click = "v:lua.ScFa" },
        { text = { " " } },
        { text = { builtin.lnumfunc }, click = "v:lua.ScLa" },
        {
          sign = {
            namespace = { "gitsign" },
            maxwidth = 1,
            colwidth = 1,
            auto = false,
          },
          click = "v:lua.ScSa",
        },
        { text = { " " } },
        {
          sign = {
            namespace = { "diagnostic" },
            maxwidth = 1,
            colwidth = 1,
            auto = false,
          },
          click = "v:lua.ScSa",
        },
        { text = { " " } },
      },
      ft_ignore = {
        "help",
        "vim",
        "fugitive",
        "alpha",
        "dashboard",
        "neo-tree",
        "Trouble",
        "noice",
        "lazy",
        "toggleterm",
      },
    })
  end,
})

-- winbar
register("Bekaboo/dropbar.nvim", {
  -- lazy loading is done in this plugin
  lazy = false,
  keys = {
    {
      "<leader>p",
      function()
        require("dropbar.api").pick()
      end,
      desc = "Open picker on winbar",
    },
  },
})

register("sainnhe/everforest", {
  lazy = true,
  init = function()
    vim.g.everforest_background = "hard"
    vim.g.everforest_enable_italic = 1
    vim.g.everforest_diagnostic_text_highlight = 1
    vim.g.everforest_diagnostic_line_highlight = 1
    vim.g.everforest_diagnostic_virtual_text = "colored"
    vim.g.everforest_current_word = "bold"

    -- Enable performance enhancment will disable color redefinition at below.
    --vim.g.everforest_better_performance = 1

    vim.api.nvim_create_autocmd("ColorScheme", {
      group = vim.api.nvim_create_augroup("custom_highlights_everforest", {}),
      pattern = "everforest",
      callback = function()
        local config = vim.fn["everforest#get_configuration"]()
        local palette = vim.fn["everforest#get_palette"](config.background, config.colors_override)
        local set_hl = vim.fn["everforest#highlight"]

        -- Get palette information by command:
        --   :lua=vim.fn["everforest#get_palette"]("hard", vim.empty_dict())
        --   :CccHighlightEnable
        set_hl("DiagnosticLineSignError", palette.none, palette.bg_red)
        set_hl("DiagnosticLineSignHint", palette.none, palette.bg4)
        set_hl("DiagnosticLineSignInfo", palette.none, palette.bg_green)
        set_hl("DiagnosticLineSignWarn", palette.none, palette.bg_yellow)

        set_hl("Headline1", palette.purple, palette.bg_visual)
        set_hl("Headline2", palette.orange, palette.bg_yellow)
        set_hl("Headline3", palette.yellow, palette.statusline2)

        set_hl("NeoTreeNormal", palette.fg, palette.bg0)
        set_hl("NeoTreeEndOfBuffer", palette.bg0, palette.bg0)
        set_hl("NeoTreeVertSplit", palette.bg0, palette.bg0)

        -- Fix DropBar color
        set_hl("WinBar", palette.grey0, palette.bg0)
        set_hl("WinBarNC", palette.grey0, palette.bg0)
      end,
    })
  end,
})
