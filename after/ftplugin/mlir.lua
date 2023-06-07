if require("libs.cache")["mlir"] then
  return
end

vim.bo.comments = vim.bo.comments .. "://"
vim.bo.commentstring = "// %s"

local is_buddy_mlir = (vim.fn.expand("%:p")):match("buddy.mlir") ~= nil
if is_buddy_mlir then
  require("lang").run_lsp("buddy_ls", {})
else
  require("lang").run_lsp("mlir_lsp_server", {})
end
