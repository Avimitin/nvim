--[[
███╗   ██╗██╗   ██╗██╗███╗   ███╗
████╗  ██║██║   ██║██║████╗ ████║
██╔██╗ ██║██║   ██║██║██╔████╔██║
██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝

Author: Avimitin
Source: https://github.com/Avimitin/nvim
License: MIT License
--]]

-- load basic configuration
local utils = require("utils")

-- Try to call the cache plugin
local cache_ok, cache_err = pcall(require, "impatient")
if not cache_ok then
  utils.errorL(cache_err, "cache")
end

for _, module_name in ipairs({ "options", "mappings", "commands", "autocmd" }) do
  local ok, err = pcall(require, module_name)
  if not ok then
    local msg = "calling module: " .. module_name .. " fail: " .. err
    utils.errorL(msg)
  end
end

-- load plugins
require("plugins").load()
