if require("libs.cache")["typescript_lsp"] then
  return
end

require("lang.tsserver").setup({})
