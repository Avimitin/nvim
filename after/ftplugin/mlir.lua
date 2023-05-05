if require("libs.cache")["mlir"] then
  return
end

vim.bo.comments = vim.bo.comments .. "://"
vim.bo.commentstring = "// %s"

require("lsp").start("mlir_lsp_server", {})
