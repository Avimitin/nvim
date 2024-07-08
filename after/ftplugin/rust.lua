if require("libs.cache")["rust_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "rust_analyzer", {})
