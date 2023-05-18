if require("libs.cache")["ocaml_lsp"] then
  return
end

require("lang").run_lsp("ocamllsp", {})
