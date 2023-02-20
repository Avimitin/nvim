if vim.b.did_load_lspconfig then
  return
end

vim.b.did_load_lspconfig = true

require("lsp.javascript").setup()
