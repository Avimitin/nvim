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
    commentStyle = { italic = true },
    functionStyle = { bold = true },
    keywordStyle = { italic = true },
    statementStyle = { bold = true },
    typeStyle = { bold = true },
    variablebuiltinStyle = { italic = true },
    specialReturn = true, -- special highlight for the return keyword
    specialException = true, -- special highlight for exception handling keywords
    transparent = false, -- do not set background color
    dimInactive = true,
    colors = {},
    overrides = {
      -- use gradient yellow for heading
      htmlH1 = {
        fg = default.waveRed,
        bold = true,
      },
      htmlH2 = {
        fg = default.surimiOrange,
        bold = true,
      },
      htmlH3 = {
        fg = default.autumnYellow,
        bold = true,
      },
      htmlH4 = {
        fg = default.carpYellow,
        bold = true,
      },
      htmlH5 = {
        fg = default.boatYellow2,
        bold = true,
      },
      htmlH6 = {
        fg = default.boatYellow1,
        italic = true,
      },
      mkdHeading = {
        fg = default.sumiInk4,
      },
      mkdListItem = {
        fg = default.surimiOrange,
        bold = true,
      },
      Todo = {
        fg = default.fujiWhite,
        bg = default.samuraiRed,
        bold = true,
      },
      Pmenu = {
        bg = default.sumiInk2,
      },
      HighLightLineMatches = {
        bg = default.winterYellow,
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
