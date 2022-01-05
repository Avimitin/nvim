-- theme
vim.opt.termguicolors = true
vim.opt.background = "dark"
-- Available theme value:
-- "ayu", "kanagawa", "deus"
local theme = "kanagawa"

local function ayu_setup()
  require('ayu').setup({
    mirage = true,
    overrides = {}
  })
end

local function deus_setup()
  vim.g.deus_background = "hard"
end

local function kanagawa_setup()
  local default = require('kanagawa.colors').setup()
  require('kanagawa').setup({
    undercurl = true, -- enable undercurls
    commentStyle = "italic",
    functionStyle = "bold",
    keywordStyle = "italic",
    statementStyle = "bold",
    typeStyle = "NONE",
    variablebuiltinStyle = "italic",
    specialReturn = true, -- special highlight for the return keyword
    specialException = true, -- special highlight for exception handling keywords
    transparent = false, -- do not set background color
    colors = {},
    overrides = {
      LazygitBackground = {
        bg = default.sumilnk3
      },
      FTermBackground = {
        bg = default.sumilnk3
      },
      htmlH1 = {
        fg = default.peachRed,
        style = 'bold'
      },
      htmlH2 = {
        fg = default.roninYellow,
        style = 'bold'
      },
      htmlH3 = {
        fg = default.autumnYellow,
        style = 'bold'
      },
      htmlH4 = {
        fg = default.autumnGreen,
        style = 'bold'
      },
      Todo = {
        fg = default.fujiWhite,
        bg = default.samuraiRed,
        style = 'bold'
      }
    }
  })
end

local theme_opt = {
  ["ayu"] = ayu_setup,
  ["deus"] = deus_setup,
  ["kanagawa"] = kanagawa_setup
}

theme_opt[theme]()

vim.cmd("colorscheme " .. theme)
