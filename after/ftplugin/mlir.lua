if require("libs.cache")["mlir"] then
  return
end

vim.bo.comments = vim.bo.comments .. "://"
vim.bo.commentstring = "// %s"

local function require_buddy_lsp()
  local buddy_projects = {
    "vector",
    "buddy-mlir",
  }

  local current_pwd = vim.fn.expand("%:p")
  for _, prj in ipairs(buddy_projects) do
    if current_pwd:match(prj) ~= nil then
      return true
    end
  end

  return false
end

local bufnr = vim.api.nvim_get_current_buf()
if require_buddy_lsp() then
  require("lang").run_lsp(bufnr, "buddy_ls", {})
else
  require("lang").run_lsp(bufnr, "mlir_lsp_server", {})
end
