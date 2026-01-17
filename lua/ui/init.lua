local register = require("pack").register

-- Deep dark purple colorscheme
register("rebelot/kanagawa.nvim", {
  rev = "aef7f5cec0a40dbe7f3304214850c472e2264b10",
  sha256 = "sha256-nHcQWTX4x4ala6+fvh4EWRVcZMNk5jZiZAwWhw03ExE=",
  config = function()
    require("ui.kanagawa")
  end,
})

--- List of nerd-font icons
register("kyazdani42/nvim-web-devicons", {
  rev = "803353450c374192393f5387b6a0176d0972b848",
  sha256 = "sha256-x1ujwUXnRolP9SRUD7/Pb4/AZu+3YpC6CfGuq3Bn6Ew=",
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
  rev = "c4801de4bad71e0a38cab8e5e703524c97496812",
  sha256 = "sha256-8K/HlOCcfdqARN+CXfd3nXcsQp8wRtW0v13pLB5SZ4Q=",
  branch = "global-status-line",
  config = function()
    require("ui.statusline")
  end,
})

-- Indent guide line
register("lukas-reineke/indent-blankline.nvim", {
  rev = "005b56001b2cb30bfa61b7986bc50657816ba4ba",
  sha256 = "sha256-0q/V+b4UrDRnaC/eRWOi9HU9a61vQSAM9/C8ZQyKt+Y=",
  config = function()
    require("ui.indent")
  end,
})

-- Notification UI
register("j-hui/fidget.nvim", {
  rev = "7fa433a83118a70fe24c1ce88d5f0bd3453c0970",
  sha256 = "sha256-Zap4UVicIvCaPqCMgdlnEAGbMzq1xM4uGpVqZL1iju0=",
  config = function()
    require("fidget").setup({})
    vim.notify = require("fidget").notify
  end,
})

-- prettify the input and select ui
register("stevearc/dressing.nvim", {
  rev = "2d7c2db2507fa3c4956142ee607431ddb2828639",
  sha256 = "sha256-dBz+/gZA6O6fJy/GSgM6ZHGAR3MTGt/W1olzzTYRlgM=",
  config = function()
    require("dressing").setup({
      input = { border = "solid", prefer_width = 15, min_width = { 10, 0.1 } },
      select = { nui = { border = "solid" } },
    })
  end,
})

register("luukvbaal/statuscol.nvim", {
  rev = "c46172d0911aa5d49ba5f39f4351d1bb7aa289cc",
  sha256 = "sha256-nFEQRJ5V+0RskJoAVRPAe2yrkORMTg9Jm13ClTmTSgk=",
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
  rev = "c85bdbb25db096fbcf616bc4e1357bd61fe2c199",
  sha256 = "sha256-Uz79FiDF1EF/IPj35PImkRuudZBARWDUEEbTdT4/Tbs=",
  config = function()
    require("smear_cursor").toggle()
  end,
})
