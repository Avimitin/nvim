if require("libs.cache")["clang_lsp_c"] then
  return
end

require("lang").run_lsp("clangd", {})
