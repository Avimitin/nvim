-- [[
-- Components:
--   autoload,          (This plugins will always be sources)
--   markdown_plugins,  (markdown plugins)
--   git_tools,         (git plugins)
--   editor_enhance,    (plugins to enhance neovim)
--   colorscheme,       (colorscheme plugins)
--   completion,        (nvim-cmp and its plugins)
--   coding_enhance,    (plugins for coding)
-- ]]

local autoload = {
  -- speed up neovim!
  {
    "nathom/filetype.nvim",
    config = function()
      require("filetype").setup({
        -- overrides the filetype or function for filetype
        -- See https://github.com/nathom/filetype.nvim#customization
        overrides = {},
      })
    end,
  },

  -- adjust the shiftwidth and expandtab settins
  {
    "tpope/vim-sleuth",
  },

  -- Fix the CursorHold performance bug
  {
    "antoinemadec/FixCursorHold.nvim",
  },

  -- cache everything!
  {
    "lewis6991/impatient.nvim",
  },
}

return {
  autoload,
}
