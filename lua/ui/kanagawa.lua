local default = require("kanagawa.colors").setup()
vim.opt.background = "dark"

local telescope_bg = "#14141d"
local overrides = {
  CmpItemKindProperty = {
    link = "@property",
  },
  CmpItemKindKeyword = { link = "@keyword.return" },
  CmpItemKindField = { link = "@field" },
  CmpDocumentation = {
    link = "Pmenu",
  },
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
  TelescopeNormal = {
    bg = telescope_bg,
  },
  TelescopePromptNormal = {
    bg = "#24242f",
  },
  TelescopePromptBorder = {
    fg = "#24242f",
    bg = "#24242f",
  },
  TelescopeBorder = {
    fg = telescope_bg,
    bg = telescope_bg,
  },
  TelescopePreviewTitle = {
    fg = telescope_bg,
    bg = telescope_bg,
  },
  TelescopePreviewBorder = {
    fg = telescope_bg,
    bg = telescope_bg,
  },
  TelescopePromptTitle = {
    fg = "#2a2a37",
    bg = default.springViolet2,
  },
  TelescopeResultsTitle = {
    bg = telescope_bg,
  },
  ["@text.reference"] = {
    fg = default.springBlue,
    italic = true,
  },
  ["@text.uri"] = {
    link = "Comment",
  },
}

if vim.cfg.ui.darker_background then
  local bg = "#0d1117"
  if type(vim.cfg.ui.darker_background) == "string" then
    bg = vim.cfg.ui.darker_background
  end
  overrides.normal = {
    bg = bg,
    fg = default.fujiWhite,
  }
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

vim.cmd("colorscheme kanagawa")
