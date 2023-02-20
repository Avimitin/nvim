if require("libs.cache")["js_lsp"] then
  return
end

require("lsp.javascript").setup()
