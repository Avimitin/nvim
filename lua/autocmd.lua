-- use relativenumber when editing
vim.cmd[[ au InsertEnter * set norelativenumber ]]
vim.cmd[[ au InsertLeave * set relativenumber ]]

vim.cmd([[autocmd BufWritePost load.lua source <afile> | PackerCompile]])
