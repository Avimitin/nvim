local present, lspconfig = pcall(require, "lspconfig")
if not present then
  vim.notify("Fail to load LSP", vim.log.levels.ERROR, {
    title = "plugins",
  })
  return
end

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
    spacing = 4,
    format = function(diagnostic)
      local icons = {
        ERROR = " ",
        WARN = " ",
        HINT = " ",
        INFO = " ",
      }
      return string.format(
        "%s %s",
        icons[vim.diagnostic.severity[diagnostic.severity]],
        diagnostic.message
      )
    end,
  },
  signs = true,
  underline = true,
  -- update diagnostic in insert mode will be annoying when the output is too verbose
  update_in_insert = false,
})

local signs = {
  Error = " ",
  Warn = " ",
  Hint = " ",
  Info = " ",
}

for type, _ in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {
    text = "",
    texthl = hl,
    numhl = hl,
    linehl = hl,
  })
end

local config = require("editor").config.lspconfig
-- Attach the above settings to all the lspservers. And tell the nvim-lsp-installer to
-- install those servers when necessary.
for _, server in pairs(config) do
  local opts = {
    on_attach = require("plugins.coding.keymap").lsp_keymap,
    capabilities = capabilities,
    root_dir = vim.loop.cwd,
    handlers = handlers,
    settings = server.settings,
  }

  lspconfig[server.name].setup(opts)
  vim.cmd([[ do User LspAttachBuffers ]])
end
