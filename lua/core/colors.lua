-- theme
vim.opt.termguicolors = true
vim.opt.background = "dark"

local M = {}

local function set_color(color_name)
  vim.cmd("colorscheme " .. color_name)
end

-- Available theme value:
-- "kanagawa", "deus"
M.theme = "kanagawa"

M.deus_setup = function()
  vim.g.deus_background = "hard"
  set_color("deus")
end

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
      }
    },
  })
  set_color("kanagawa")
end

return M
