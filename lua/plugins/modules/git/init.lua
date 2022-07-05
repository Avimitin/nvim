local register = require("plugins").register
local config = require("plugins.modules.git.config")

config.pre()

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

  -- Call the lazygit inside neovim, relies on lazygit executable
  {
    "kdheepak/lazygit.nvim",
    cmd = "LazyGit",
  },

  -- Show git information in neovim
  {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    event = "BufRead",
    config = config.gitsigns_config,
  },

  -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
  {
    "sindrets/diffview.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("diffview").setup({
        -- see configuration in
        -- https://github.com/sindrets/diffview.nvim#configuration
      })
    end,
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
  },

  -- Flog is a lightweight and powerful git branch viewer that integrates with fugitive.
  {
    "rbong/vim-flog",
    -- please don't use this plugin directly, I embed it with vim-fugitive
    -- See lua/core/commands.lua
    opt = true,
  },
}

-- register plugins repository
register(repos)

-- initialize commands
require("plugins.modules.git.commands")
