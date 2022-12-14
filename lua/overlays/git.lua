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

  -- A git log viewer, depends on fugitive
  {
    "rbong/vim-flog",
    opt = true,
  },

  -- Show git information in neovim
  {
    "lewis6991/gitsigns.nvim",
    opt = true,
    rc = "gitsigns",
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
