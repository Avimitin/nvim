-- use relativenumber when editing
vim.cmd([[ au InsertEnter * set norelativenumber ]])
vim.cmd([[ au InsertLeave * set relativenumber ]])

-- auto compile when editing the load.lua file
vim.cmd([[autocmd BufWritePost load.lua source <afile> | PackerCompile]])

-- start insert when enter the terminal
vim.cmd("autocmd TermOpen term://* startinsert")
