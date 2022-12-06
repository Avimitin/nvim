vim.g.deus_background = "hard"

local colorscheme = {
  -- dark green color scheme
  {
    "Avimitin/neovim-deus",
    cond = function()
      return vim.g.nvcfg.ui.theme == "deus"
    end,
    config = function()
      vim.cmd("colorscheme deus")
    end,
  },

  -- dark purple color scheme
  {
    "rebelot/kanagawa.nvim",
    cond = function()
      return vim.g.nvcfg.ui.theme == "kanagawa"
    end,
    rc = "kanagawa",
  },

  -- GitHub light and dark colorscheme
  {
    "projekt0n/github-nvim-theme",
    cond = function()
      return vim.tbl_contains({
        "github_dark",
        "github_dark_default",
        "github_dimmed",
        "github_light",
        "github_light_default",
      }, vim.g.nvcfg.ui.theme)
    end,
    rc = "github_theme",
  },
}

return colorscheme
