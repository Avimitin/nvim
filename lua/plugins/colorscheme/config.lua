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
  theme = require("editor").config.theme,
}

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
  local overrides = {
    -- use gradient yellow for heading
    markdownH1 = {
      fg = default.waveRed,
      bold = true,
    },
    markdownH2 = {
      fg = default.surimiOrange,
      bold = true,
    },
    markdownH3 = {
      fg = default.autumnYellow,
      bold = true,
    },
    markdownH4 = {
      fg = default.carpYellow,
      bold = true,
    },
    markdownH5 = {
      fg = default.boatYellow2,
      bold = true,
    },
    markdownH6 = {
      fg = default.boatYellow1,
      italic = true,
    },
    markdownH1Delimiter = {
      fg = default.sumiInk4,
    },
    markdownH2Delimiter = {
      fg = default.sumiInk4,
    },
    markdownH3Delimiter = {
      fg = default.sumiInk4,
    },
    markdownH4Delimiter = {
      fg = default.sumiInk4,
    },
    markdownH5Delimiter = {
      fg = default.sumiInk4,
    },
    markdownH6Delimiter = {
      fg = default.sumiInk4,
    },
    markdownListMarker = {
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
    WinSeparator = {
      fg = default.sumiInk4,
    },
    DiagnosticSignError = {
      bg = "#2A1C23",
    },
    DiagnosticSignHint = {
      bg = "#1C1E2A",
    },
    DiagnosticSignWarn = {
      bg = "#2F261A",
    },
    DiagnosticSignInfo = {
      bg = "#262729",
    },
  }

  local present, custom = pcall(require, "custom")
  if present and custom.use_darker_background ~= nil then
    local background = nil

    if type(custom.use_darker_background) == "boolean" and custom.use_darker_background then
      background = "#14141D"
    end

    if type(custom.use_darker_background) == "string" then
      background = custom.use_darker_background
    end

    if background ~= nil then
      overrides.normal = {
        bg = background,
        fg = default.fujiWhite,
      }
    end
  end

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
    dimInactive = false,
    colors = {},
    globalStatus = true,
    overrides = overrides,
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
        DiagnosticError = {
          bg = "#2A1C23",
        },
        DiagnosticHint = {
          bg = "#1C1E2A",
        },
        DiagnosticWarn = {
          bg = "#1C140D",
        },
        DiagnosticInfo = {
          bg = "#262729",
        },
      }
    end,
  })
end

-- return the configuration for load condition
return colorscheme_settings
