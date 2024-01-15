if require("libs.cache")["ocaml_lsp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "ocamllsp", {})
