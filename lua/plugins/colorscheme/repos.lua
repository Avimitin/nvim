local colorscheme = {
  -- dark green color scheme
  {
    "Avimitin/neovim-deus",
    cond = function()
      -- must explicit call this module, or packer will compile it to a constant value
      return require("plugins.colorscheme.config").theme == "deus"
    end,
    config = function()
      require("plugins.colorscheme.config").deus_setup()
    end,
  },

  -- dark purple color scheme
  {
    "rebelot/kanagawa.nvim",
    cond = function()
      return require("plugins.colorscheme.config").theme == "kanagawa"
    end,
    config = function()
      require("plugins.colorscheme.config").kanagawa_setup()
    end,
  },

  -- GitHub light and dark colorscheme
  {
    "projekt0n/github-nvim-theme",
    cond = function()
      local select = require("plugins.colorscheme.config").theme
      for _, avail in ipairs({
        "github_dark",
        "github_dark_default",
        "github_dimmed",
        "github_light",
        "github_light_default",
      }) do
        if select == avail then
          return true
        end
      end
      return false
    end,
    config = function()
      require("plugins.colorscheme.config").github_setup()
    end,
  },
}

return colorscheme
