local register = require("pack").register

-- Deep dark purple colorscheme
register("rebelot/kanagawa.nvim", {
  config = function()
    require("ui.kanagawa")
  end,
})

--- List of nerd-font icons
register("kyazdani42/nvim-web-devicons", {
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
  config = function()
    require("ui.statusline")
  end,
})

-- Indent guide line
register("lukas-reineke/indent-blankline.nvim", {
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

-- prettify the input and select ui
register("stevearc/dressing.nvim", {
  config = function()
    require("dressing").setup({
      input = { border = "solid", prefer_width = 15, min_width = { 10, 0.1 } },
      select = { nui = { border = "solid" } },
    })
  end,
})

register("luukvbaal/statuscol.nvim", {
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

register("sphamba/smear-cursor.nvim", {
  config = function()
    require("smear_cursor").toggle()
  end,
})