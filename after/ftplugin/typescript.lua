if require("libs.cache")["typescript_lsp"] then
  return
end

require("lsp.javascript").setup()
