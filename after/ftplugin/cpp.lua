if require("libs.cache")["clang_lsp_cpp"] then
  return
end

local bufnr = vim.api.nvim_get_current_buf()
require("lang").run_lsp(bufnr, "ccls", {
  root_dir = require("lspconfig").util.root_pattern(
    "build",
    "compile_commands.json",
    ".ccls",
    ".git"
  ),
  init_options = {
    compilationDatabaseDirectory = "build",
    index = {
      threads = 0,
    },
    clang = {
      excludeArgs = { "-frounding-math" },
    },
  },
})
