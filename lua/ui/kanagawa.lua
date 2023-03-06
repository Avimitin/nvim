vim.opt.background = "dark"

local function override(colors)
  local default = colors.palette
  local theme = colors.theme

  local background = default.sumiInk1
  if vim.cfg.ui.darker_background then
    if type(vim.cfg.ui.darker_background) == "string" then
      background = vim.cfg.ui.darker_background
    end

    background = "#0d1117"
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
    Normal = {
      bg = background,
      fg = default.fujiWhite,
    },
    Pmenu = { bg = default.sumiInk3 },
    TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },
    TelescopePreviewNormal = { bg = theme.ui.bg_dim },
    TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
    TelescopePromptNormal = { bg = theme.ui.bg_p1 },
    TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
    TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
    TelescopeTitle = { fg = theme.ui.special, bold = true },
    WinSeparator = { fg = default.sumiInk4 },
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
