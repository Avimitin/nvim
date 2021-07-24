--[[
███╗   ██╗██╗   ██╗██╗███╗   ███╗
████╗  ██║██║   ██║██║████╗ ████║
██╔██╗ ██║██║   ██║██║██╔████╔██║
██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝

Author: Avimitin
Source: https://github.com/Avimitn/nvim
Credit:
	This project is inspired by the following open-source projects:
		* https://github.com/theniceboy/nvim
		* https://github.com/siduck76/NvChad
License:
	MIT License
--]]

-- Loading basic configuration
require('options')
require('mapping')

-- detecting plugin manager
local has_packer_file = pcall(vim.cmd, [[packadd packer.nvim]])
local has_packer, error_msg = pcall(require, 'packer')

local no_plugins = false

if not has_packer and not has_packer_file then
	local install_path = vim.fn.stdpath("data").."/site/pack/packer/opt/packer.nvim"
	print("Installing packer to "..install_path)

	vim.fn.delete(install_path, "rf")
	vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)

	no_plugins = true

	vim.cmd[[packadd packer.nvim]]

	has_packer, error_msg = pcall(require, 'packer')
	if not has_packer then
		print(error_msg)
		return
	end -- inner has_packer

end -- outer has_packer

-- Reading plugins configuration
local ok, error = pcall(require, 'plug')
if not ok then
	print("failed to initialize plugin")
	print(error)
end

vim.cmd([[autocmd BufWritePost plug.lua source <afile> | PackerCompile]])

if no_plugins then
	vim.cmd([[PackerSync]])
	print("Please quit the neovim after plugins are all successful installed")
end

