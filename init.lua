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
local utils = require("core.utils")

-- Try to call the cache plugin
pcall(require, "impatient")

for _, module_name in ipairs({
  "core.options",
  "mappings",
  "core.commands",
  "core.autocmd",
  "plugins.options",
}) do
  local ok, err = pcall(require, module_name)
  if not ok then
    local msg = "calling module: " .. module_name .. " fail: " .. err
    utils.errorL(msg)
  end
end

-- since we have packer compiled, we don't need to load this immediately
vim.defer_fn(function()
  require("plugins").load()
end, 0)
