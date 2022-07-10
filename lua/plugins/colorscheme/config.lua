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
--
--
-- Available theme value:
--  "kanagawa", "deus", "github_{light,dark}"
local colorscheme_settings = {
  theme = "kanagawa",
}

-- if the custom files is created
local ok, custom = pcall(require, "custom")
if ok and custom and custom.theme then
  colorscheme_settings.theme = custom.theme
end

-- This functions finally apply the colorscheme
local function apply()
  vim.cmd("colorscheme " .. colorscheme_settings.theme)
end

-- configure the deus theme
colorscheme_settings.deus_setup = function()
  vim.g.deus_background = "hard"
  apply()
end

-- configure the kanagawa theme
colorscheme_settings.kanagawa_setup = function()
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

--
-- GitHub colorscheme settings
--
-- this plugin will setup colorscheme for us, no need to call colorscheme ourselves
colorscheme_settings.github_setup = function()
  -- trim the prefix text
  local theme = colorscheme_settings.theme:gsub("github_", "")
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
end

-- return the configuration for load condition
return colorscheme_settings
