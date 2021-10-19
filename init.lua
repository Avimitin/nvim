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
--]] -- Loading basic configuration
require('options')
require('keymap')

-- detecting plugin manager
local no_packer = false
local fn = vim.fn
local install_path = vim.fn.stdpath("data") ..
                         "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    print("Installing packer to " .. install_path)
    no_packer = fn.system({
        'git', 'clone', '--depth', '1',
        'https://github.com/wbthomason/packer.nvim', install_path
    })
end

local packer_call, error_msg = pcall(vim.cmd, [[packadd packer.nvim]])
if not packer_call then
    print(error_msg)
    return
end -- inner has_packer

-- Reading plugins configuration
local ok, error = pcall(require, 'plug')
if not ok then
    print("failed to initialize plugin")
    print(error)
end

vim.cmd([[autocmd BufWritePost plug.lua source <afile> | PackerCompile]])

if no_packer then require('packer').sync() end
