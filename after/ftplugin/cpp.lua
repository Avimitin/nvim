if require("libs.cache")["clang_lsp"] then
  return
end

require("lang").run_lsp("clangd", {})
