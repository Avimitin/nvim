if require("libs.cache")["clang_lsp_cpp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "clangd", {})
