if require("libs.cache")["typescript_lsp"] then
  return
end

if vim.cfg.typescript.server == "denols" then
  -- fix highlight on codefences
  vim.g.markdown_fenced_languages = {
    "ts=typescript",
  }

  local config = require("lsp.deno")
  require("lsp").start(vim.cfg.typescript.server, config)
elseif vim.cfg.typescript.server == "tsserver" then
  require("lsp.javascript").setup()
end
