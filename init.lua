-- Add support for loading lua package from nix store
if vim.fn.expand("$NIX_PROFILES") ~= "$NIX_PROFILES" then
  local append = function(path)
    package.path = package.path .. ";" .. path
  end
  for _, p in ipairs(vim.split(vim.fn.expand("$NIX_PROFILES"), " ")) do
    append(p .. "/share/lua/5.1/?/init.lua")
    append(p .. "/share/lua/5.1/?.lua")
  end
end

require("key-mapping")

require("core")
require("pack").setup()
