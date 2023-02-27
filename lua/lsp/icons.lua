local Export = {}

function Export.setup()
  -- Setup diagnostic icons and signs
  vim.diagnostic.config({
    virtual_text = {
      prefix = "",
      spacing = 6,
      format = function(diagnostic)
        return string.format(
          "%s  %s",
          vim.cfg.icons[vim.diagnostic.severity[diagnostic.severity]],
          diagnostic.message
        )
      end,
    },
    signs = true,
    underline = true,
    -- update diagnostic in insert mode will be annoying when the output is too verbose
    update_in_insert = false,
  })

  local types = {
    "Error",
    "Warn",
    "Hint",
    "Info",
  }

  for _, diag_type in ipairs(types) do
    local hl = "DiagnosticSign" .. diag_type
    vim.fn.sign_define(hl, {
      text = "",
      texthl = hl,
      numhl = hl,
      linehl = hl,
    })
  end
end

return Export
