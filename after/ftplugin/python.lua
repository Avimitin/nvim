if require("libs.cache")["python_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "pyright", {})
