vim.g.dashboard_preview_command = 'cat'
vim.g.dashboard_preview_pipeline = 'lolcat'
vim.g.dashboard_preview_file = "~/.config/nvim/preview.cat"
vim.g.dashboard_preview_file_height = 11
vim.g.dashboard_preview_file_width = 50
vim.g.dashboard_disable_statusline = 1
vim.g.dashboard_default_executive = "telescope"

vim.cmd[[
autocmd FileType dashboard set showtabline=0 laststatus=0
autocmd WinLeave <buffer> set showtabline=2 laststatus=2
]]
