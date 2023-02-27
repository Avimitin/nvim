if require("libs.cache")["js_lsp"] then
  return
end

local opt = vim.cfg.javascript
if not opt.enable then
  return
end

if opt.server == "tsserver" then
  require("lsp.tsserver").setup(opt)
elseif opt.server == "denols" then
  require("lsp.deno").setup()
end
