if require("libs.cache")["js_lsp"] then
  return
end

require("lang.tsserver").setup({})
