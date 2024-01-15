if require("libs.cache")["tsx"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "tsserver", {})
