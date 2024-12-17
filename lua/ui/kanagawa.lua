local function override(colors)
  local default = colors.palette
  -- local theme = colors.theme

  local overrides = {
    -- CursorLine = { bg = "#1b1b26" },
    --
    Normal = { bg = default.sumiInk0, fg = default.fujiWhite },
    TelescopeBorder = { fg = default.sumiInk0, bg = default.sumiInk0 },
    NormalFloat = { bg = default.sumiInk1 },
    BufferManagerBorder = { bg = default.sumiInk1, fg = default.sumiInk1 },
    -- FloatTitle = { fg = "#D3C6AA", bg = "#5C3F4F", bold = true },
    -- FloatBorder = { link = "NormalFloat" },

    -- Pmenu = { bg = default.sumiInk3 },
    CmpDocumentation = { link = "Pmenu" },

    ["@markup.heading.1"] = {
      fg = default.springGreen,
      bold = true,
    },
    ["@markup.heading.2"] = {
      fg = default.waveAqua1,
      bold = true,
    },
    ["@markup.heading.3"] = {
      fg = default.peachRed,
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
})
