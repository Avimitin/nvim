-- trim the prefix text
local theme = vim.g.nvcfg.ui.theme:gsub("github_", "")

vim.o.background = "dark"

if theme:find("light") then
  vim.o.background = "light"
end

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
