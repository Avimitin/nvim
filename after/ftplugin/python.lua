if require("libs.cache")["python_lsp"] then
  return
end

require("lang").run_lsp("pyright", {})
