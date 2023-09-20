vim.opt.background = "dark"

local function override(colors)
  local default = colors.palette
  local theme = colors.theme

  local background = default.sumiInk1
  if vim.cfg.ui.darker_background then
    if type(vim.cfg.ui.darker_background) == "string" then
      background = vim.cfg.ui.darker_background
    else
      background = "#0d1117"
    end
  end

  local overrides = {
    CmpDocumentation = { link = "Pmenu" },
    CmpItemKindField = { link = "@field" },
    CmpItemKindKeyword = { link = "@keyword.return" },
    CmpItemKindProperty = { link = "@property" },
    DiagnosticSignError = { bg = "#2A1C23" },
    DiagnosticSignHint = { bg = "#1C1E2A" },
    DiagnosticSignInfo = { bg = "#262729" },
    DiagnosticSignWarn = { bg = "#2F261A" },
    GitSignsAdd = { bg = background },
    GitSignsChange = { bg = background },
    GitSignsDelete = { bg = background },
    HighLightLineMatches = { bg = default.winterYellow },
    --[[ LeapBackdrop = { fg = default.dragonBlue },
    LeapMatch = { fg = default.fujiWhite, bold = true, nocombine = true },
    LeapLabelPrimary = {
      fg = default.sumiInk4,
      bg = default.roninYellow,
      bold = true,
      nocombine = true,
    },
    LeapLabelSecondary = {
      fg = default.springBlue,
      bold = true,
      nocombine = true,
    }, ]]
    Normal = {
      bg = background,
      fg = default.fujiWhite,
    },
    Pmenu = { bg = default.sumiInk3 },
    -- depth 1
    RainbowDelimiterRed = { fg = default.springViolet2 },
    -- depth 2
    RainbowDelimiterYellow = { fg = default.dragonBlue },
    -- depth 3
    RainbowDelimiterBlue = { fg = default.surimiOrange },
    -- depth 4
    RainbowDelimiterGreen = { fg = default.springGreen },
    -- depth 5
    RainbowDelimiterCyan = { fg = default.waveAqua2 },
    -- depth 6
    RainbowDelimiterOrange = { fg = default.springViolet1 },
    -- depth 7
    RainbowDelimiterViolet = { fg = default.springViolet2 },
    TelescopePromptNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_p1 },
    TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
    TelescopePromptTitle = { fg = theme.ui.fg_dim, bg = theme.ui.bg_p1 },
    TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },
    TelescopePreviewNormal = { bg = theme.ui.bg_dim },
    TelescopeResultsBorder = { fg = theme.ui.bg_dim, bg = theme.ui.bg_dim },
    TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_dim },
    TelescopeResultsTitle = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
    TelescopeTitle = { fg = theme.ui.special, bold = true },
    WinSeparator = { fg = default.sumiInk4 },
    ["@text.title.1"] = {
      fg = default.peachRed,
      bold = true,
    },
    ["@text.title.2"] = {
      fg = default.surimiOrange,
      bold = true,
    },
    ["@text.title.3"] = {
      fg = default.carpYellow,
      bold = true,
    },
    ["@text.title"] = {
      fg = default.crystalBlue,
      bold = true,
    },
    ["@text.reference"] = {
      fg = default.springBlue,
      italic = true,
    },
    ["@text.uri"] = {
      link = "Comment",
    },
  }

  return overrides
end

require("kanagawa").setup({
  undercurl = true, -- enable undercurls
  commentStyle = { italic = true },
  functionStyle = { bold = true },
  keywordStyle = { italic = true },
  statementStyle = { bold = true },
  typeStyle = { bold = true },
  variablebuiltinStyle = { italic = true },
  globalStatus = true,
  overrides = override,
  colors = {
    theme = {
      all = {
        ui = {
          bg_gutter = "none",
        },
      },
    },
  },
  background = {
    dark = "wave",
    light = "lotus",
  },
})

vim.cmd("colorscheme kanagawa")
