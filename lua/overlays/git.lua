local gitsign = require("overlays.rc.gitsigns")

local repos = {
  -- A git tool like magit in Emacs
  {
    "tpope/vim-fugitive",
    cmd = {
      "G",
      "Git",
      "Ggrep",
      "Gdiffsplit",
      "GBrowse",
    },
  },

  -- Show git information in neovim
  {
    "lewis6991/gitsigns.nvim",
    opt = true,
    config = gitsign.config,
    setup = gitsign.setup,
  },

  -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
  {
    "sindrets/diffview.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("diffview").setup({})
    end,
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
  },
}

return repos
