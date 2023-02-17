-- Gets a new ClientCapabilities object describing the LSP client
-- capabilities.
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem = {
  documentationFormat = {
    "markdown",
    "plaintext",
  },
  snippetSupport = true,
  preselectSupport = true,
  insertReplaceSupport = true,
  labelDetailsSupport = true,
  deprecatedSupport = true,
  commitCharactersSupport = true,
  tagSupport = {
    valueSet = { 1 },
  },
  resolveSupport = {
    properties = {
      "documentation",
      "detail",
      "additionalTextEdits",
    },
  },
}

-- Setup border for the floating window
local handlers = {
  ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "single",
  }),
  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "single",
  }),
}

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

return {
  capabilities = capabilities,
  root_dir = vim.loop.cwd,
  handlers = handlers,
}
