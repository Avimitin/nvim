if require("libs.cache")["typescript_lsp"] then
  return
end

local opt = vim.cfg.typescript
if not opt.enable then
  return
end

if opt.server == "tsserver" then
  require("lang.tsserver").setup(opt)
elseif opt.server == "denols" then
  require("lang.deno").setup()
end
