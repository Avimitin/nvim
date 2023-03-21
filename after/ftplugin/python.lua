if require("libs.cache")["python_lsp"] then
  return
end

require("lsp").start("pyright", {})
