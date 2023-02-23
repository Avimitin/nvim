if require("libs.cache")["typescript_lsp"] then
  return
end

if vim.cfg.typescript.server == "denols" then
  require("lsp.deno").setup()
elseif vim.cfg.typescript.server == "tsserver" then
  require("lsp.javascript").setup()
end
