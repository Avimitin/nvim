if require("libs.cache")["ocaml_lsp"] then
  return
end

require("lsp").start("ocamllsp", {})
