if require("libs.cache")["jsx_lsp"] then
  return
end

require("lsp.javascript").setup()
