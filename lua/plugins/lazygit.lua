-- use fterm to open the lazygit
local term = require("FTerm.terminal")
local lazygit = term:new():setup({
    cmd = "lazygit",
    dimensions = {height = 0.9, width = 0.9}
})
function _G.fterm_lazygit() lazygit:toggle() end

vim.api.nvim_set_keymap('n', '<C-g>', ':lua _G.fterm_lazygit()<CR>',
                        {noremap = true, silent = true})
