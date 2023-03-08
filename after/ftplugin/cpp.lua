if require("libs.cache")["clang_lsp"] then
  return
end

require("lsp").start("clangd", {})
