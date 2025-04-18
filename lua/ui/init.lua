local register = require("pack").register

register("projekt0n/github-nvim-theme", {
  lazy = true,
})

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
        ["mill"] = {
          icon = "",
          color = "#cc3e44",
          cterm_color = "167",
          name = "Scala",
        },
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
  event = { "FileType", "WinEnter", "BufEnter" },
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
register("j-hui/fidget.nvim", {
  config = function()
    require("fidget").setup({})
    vim.notify = require("fidget").notify
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
      require("dressing").setup({
        input = { border = "solid" },
        select = { nui = { border = "solid" } },
      })
      return vim.ui.select(...)
    end
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.ui.input = function(...)
      require("lazy").load({ plugins = { "dressing.nvim" } })
      require("dressing").setup({
        input = { border = "solid", prefer_width = 15, min_width = { 10, 0.1 } },
        select = { nui = { border = "solid" } },
      })
      return vim.ui.input(...)
    end
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
        -- one space gap
        { text = { " " } },
        -- Show the fold icon
        { text = { builtin.foldfunc }, click = "v:lua.ScFa" },
        { text = { " " } },
        -- Show git status
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
        -- Show the number column
        { text = { builtin.lnumfunc }, click = "v:lua.ScLa" },
        -- Padding
        { text = { " " } },
      },
      ft_ignore = {
        "aerial",
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

register("karb94/neoscroll.nvim", {
  keys = {
    "<c-u>",
    "<c-d>",
    "<c-b>",
    "<c-f>",
    "<c-y>",
    "<c-e>",
    "zt",
    "zz",
    "zb",
  },
  config = function()
    require("neoscroll").setup({})
  end,
})
