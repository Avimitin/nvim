if require("libs.cache")["nix"] then
  return
end

require("lang").run_lsp("nil_ls", {})
