-- use fterm to open the lazygit
local term = require("FTerm.terminal")
local lazygit = term:new():setup({
    cmd = "lazygit",
    dimensions = {height = 0.96, width = 0.98}
})
function _G.fterm_lazygit() lazygit:toggle() end

vim.cmd([[command Lg lua _G.fterm_lazygit()]])
