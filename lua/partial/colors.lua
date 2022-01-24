return {
  {
    'Avimitin/neovim-deus',
    cond = function()
      return require("colors").theme == "deus"
    end,
    config = function()
      require("colors").deus_setup()
    end
  },

  {
    'Shatur/neovim-ayu',
    cond = function()
      return require("colors").theme == "ayu"
    end,
    config = function()
      require("colors").ayu_setup()
    end
  },

  {
    'rebelot/kanagawa.nvim',
    cond = function()
      return require("colors").theme == "kanagawa"
    end,
    config = function()
      require("colors").kanagawa_setup()
    end
  },

  {
    'sainnhe/everforest',
    cond = function()
      return require("colors").theme == "everforest"
    end,
    config = function()
      require("colors").everforest_setup()
    end
  },

  {
    'morhetz/gruvbox',
    cond = function()
      return require("colors").theme == "gruvbox"
    end,
    config = function()
      require("colors").gruvbox_setup()
    end
  }
}
