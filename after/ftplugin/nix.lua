if require("libs.cache")["nix"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "nil_ls", {})
