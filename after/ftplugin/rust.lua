if require("libs.cache")["rust_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()

-- setup lsp key mappings
require("lang.on_attach").setup_all(nil, bufnr)
