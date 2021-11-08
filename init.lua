--[[
███╗   ██╗██╗   ██╗██╗███╗   ███╗
████╗  ██║██║   ██║██║████╗ ████║
██╔██╗ ██║██║   ██║██║██╔████╔██║
██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝

Author: Avimitin
Source: https://github.com/Avimitin/nvim
Credit:
	This project is inspired by the following open-source projects:
		* https://github.com/theniceboy/nvim
		* https://github.com/siduck76/NvChad
License:
	MIT License
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

-- Run rooter when it is the first time enter the neovim
vim.cmd[[autocmd VimEnter * Rooter]]
