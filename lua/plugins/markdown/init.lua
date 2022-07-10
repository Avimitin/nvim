local register = require("plugins").register
local config = require("plugins.markdown.config")

config.pre()

local repos = {
  -- markdown toc
  {
    "mzlogin/vim-markdown-toc",
    cmd = {
      "GenTocGFM",
    },
  },

  -- markdown preview
  {
    "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
    ft = {
      "markdown",
    },
  },

  -- markdown editing enhancement
  {
    "plasticboy/vim-markdown",
    ft = {
      "markdown",
    },
  },

  -- table editing enhancement
  {
    "dhruvasagar/vim-table-mode",
    cmd = "TableModeToggle",
  },
}

register(repos)
