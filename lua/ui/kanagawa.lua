local function override(colors)
  local default = colors.palette
  -- local theme = colors.theme

  local overrides = {
    -- CursorLine = { bg = "#1b1b26" },
    --
    -- Normal = { bg = "#101017", fg = default.fujiWhite },
    -- NormalFloat = { bg = "#1f1f2c" },
    -- BufferManagerBorder = { bg = "#1f1f2c", fg = "#1f1f2c" },
    -- FloatTitle = { fg = "#D3C6AA", bg = "#5C3F4F", bold = true },
    -- FloatBorder = { link = "NormalFloat" },

    -- Pmenu = { bg = default.sumiInk3 },
    CmpDocumentation = { link = "Pmenu" },

    ["@markup.heading.1"] = {
      fg = default.peachRed,
      bg = default.winterRed,
      bold = true,
    },
    ["@markup.heading.2"] = {
      fg = default.roninYellow,
      bg = default.winterYellow,
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
