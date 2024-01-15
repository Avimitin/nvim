if require("libs.cache")["jsx_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "tsserver", {})
