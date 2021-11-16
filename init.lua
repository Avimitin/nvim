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
local utils = require('utils')

for _, module_name in ipairs({'options', 'keymap', 'commands'}) do
  local ok, err = pcall(require, module_name)
  if not ok then
    local msg = "calling module: "..module_name.." fail: "..err
    utils.log_err(msg)
  end
end

-- load plugins
utils.load_plugins()
