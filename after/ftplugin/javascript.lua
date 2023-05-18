if require("libs.cache")["js_lsp"] then
  return
end

local opt = vim.cfg.javascript
if not opt.enable then
  return
end

if opt.server == "tsserver" then
  require("lang.tsserver").setup(opt)
elseif opt.server == "denols" then
  require("lang.deno").setup()
end
