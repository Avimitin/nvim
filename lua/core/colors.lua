-- theme
vim.opt.termguicolors = true
vim.opt.background = "dark"

local M = {
  -- "kanagawa" by default
  -- Available theme value:
  --  "kanagawa", "deus", "night","dawn","day","nord","dusk"+"fox"
  theme = "kanagawa",
}

-- Try to update the theme value if the lua/custom.lua file exist.
-- User should return their custom value in the below form:
--
-- ```lua
-- local M = {
--     theme = "deus",
-- }
--
-- return M
-- ```

local ok, custom = pcall(require, "custom")
-- if file exist, return table exist and return table has `theme` field
if ok and custom and custom.theme then
  M.theme = custom.theme
end

-- This functions finally apply the colorscheme
local function apply()
  vim.cmd("colorscheme " .. M.theme)
end

-- configure the deus theme
M.deus_setup = function()
  vim.g.deus_background = "hard"
  apply()
end

-- configure the kanagawa theme
M.kanagawa_setup = function()
  local default = require("kanagawa.colors").setup()
  require("kanagawa").setup({
    undercurl = true, -- enable undercurls
    commentStyle = "italic",
    functionStyle = "bold",
    keywordStyle = "italic",
    statementStyle = "bold",
    typeStyle = "bold",
    variablebuiltinStyle = "italic",
    specialReturn = true, -- special highlight for the return keyword
    specialException = true, -- special highlight for exception handling keywords
    transparent = false, -- do not set background color
    dimInactive = true,
    colors = {},
    overrides = {
      htmlH1 = {
        fg = default.peachRed,
        style = "bold",
      },
      htmlH2 = {
        fg = default.roninYellow,
        style = "bold",
      },
      htmlH3 = {
        fg = default.autumnYellow,
        style = "bold",
      },
      htmlH4 = {
        fg = default.autumnGreen,
        style = "bold",
      },
      Todo = {
        fg = default.fujiWhite,
        bg = default.samuraiRed,
        style = "bold",
      },
      Pmenu = {
        bg = default.sumiInk1,
      },
      HighLightLineMatches = {
        bg = default.winterYellow,
      },
    },
  })
  apply()
end

-- configure the nightfox theme
M.nightfox_setup = function()
  require("nightfox").setup({
    options = {
      -- Compiled file's destination location
      compile_path = vim.fn.stdpath("cache") .. "/nightfox",
      compile_file_suffix = "_compiled", -- Compiled file suffix
      transparent = false, -- Disable setting background
      terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
      dim_inactive = true, -- Non focused panes set to alternative background
      styles = { -- Style to be applied to different syntax groups
        comments = "italic", -- Value is any valid attr-list value `:help attr-list`
        functions = "bold",
        keywords = "italic",
        numbers = "NONE",
        strings = "NONE",
        types = "italic,bold",
        variables = "NONE",
      },
    },
    groups = {
      HighLightLineMatches = {
        bg = "#FFDE83",
      },
    },
  })
  apply()
end

-- configure the nightfox theme
M.github_setup = function()
  -- trim the prefix text
  local theme = M.theme:gsub("github_", "")
  require("github-theme").setup({
    theme_style = theme,
    function_style = "bold",
    comment_style = "italic",
    keyword_style = "italic",
    variable_style = "NONE",
    sidebars = { "qf", "vista_kind", "terminal", "packer", "NvimTree" },

    -- Overwrite the highlight groups
    overrides = function(_)
      return {
        HighLightLineMatches = {
          bg = "#FFDE83",
        },
      }
    end,
  })
  -- this plugin will setup colorscheme for us
  -- apply()
end

-- return the configuration for load condition
return M
