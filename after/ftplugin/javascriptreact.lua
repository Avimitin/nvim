if require("libs.cache")["jsx_lsp"] then
  return
end

local opt = vim.cfg.javascriptreact
if not opt.enable then
  return
end

if opt.server == "tsserver" then
  require("lsp.tsserver").setup(opt)
elseif opt.server == "denols" then
  require("lsp.deno").setup()
end
