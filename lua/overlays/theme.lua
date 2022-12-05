local colorc = require("overlays.rc").theme

local colorscheme = {
  -- dark green color scheme
  {
    "Avimitin/neovim-deus",
    cond = function()
      return vim.g.nvcfg.theme == "deus"
    end,
    config = colorc.deus,
  },

  -- dark purple color scheme
  {
    "rebelot/kanagawa.nvim",
    cond = function()
      return vim.g.nvcfg.theme == "kanagawa"
    end,
    config = colorc.kanagawa,
  },

  -- GitHub light and dark colorscheme
  {
    "projekt0n/github-nvim-theme",
    cond = function()
      return vim.tbl_contains(vim.g.nvcfg.theme, {
        "github_dark",
        "github_dark_default",
        "github_dimmed",
        "github_light",
        "github_light_default",
      })
    end,
    config = colorc.github,
  },
}

return colorscheme
