vim.opt.background = "dark"

local enable_transparent = false
if enable_transparent then
  vim.g.neovide_transparency = 0.8
end

local function override(colors)
  local default = colors.palette
  local theme = colors.theme

  local overrides = {
    CursorLine = { bg = "#1b1b26" },

    Normal = { bg = "#101017", fg = default.fujiWhite },
    NormalFloat = { bg = "#1f1f2c" },

    Pmenu = { bg = default.sumiInk3 },
    CmpDocumentation = { link = "Pmenu" },
    CmpItemKindField = { fg = default.surimiOrange },

    LspReferenceWrite = { link = "LspReferenceText" },
    LspReferenceRead = { link = "LspReferenceText" },
    LspReferenceText = { fg = "None", bg = "None", bold = true },

    DiagnosticVirtualTextWarn = { fg = "#b7a37d" },
    DiagnosticVirtualTextInfo = { fg = "#6085b8" },
    DiagnosticVirtualTextError = { fg = "#af6b79" },
    DiagnosticVirtualTextHint = { fg = "#738a6a" },

    LeapBackdrop = { fg = "#777777" },

    HighLightLineMatches = { bg = default.winterYellow },

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

    CodeBlock = { bg = default.sumiInk1 },
    Headline1 = { fg = default.peachRed, bg = "#1c0519" },
    Headline2 = { fg = default.surimiOrange, bg = "#151F2D" },
    Headline3 = { fg = default.carpYellow, bg = default.sumiInk3 },
    ["@markup.heading.1"] = {
      fg = default.peachRed,
      bold = true,
    },
    ["@markup.heading.2"] = {
      fg = default.surimiOrange,
      bold = true,
    },
    ["@markup.heading.3"] = {
      fg = default.carpYellow,
      bold = true,
    },
    ["@markup.heading"] = {
      fg = default.crystalBlue,
      bold = true,
    },
    ["@text.reference"] = {
      fg = default.springBlue,
      italic = true,
    },
  }

  if enable_transparent then
    overrides["Normal"] = nil
  end

  return overrides
end

require("kanagawa").setup({
  undercurl = true, -- enable undercurls
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
  transparent = enable_transparent,
})
