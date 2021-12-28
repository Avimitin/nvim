-- use fterm to open the lazygit
local M = {}
local term = require("FTerm.terminal")
M.toggle = function()
  term:new():setup({
    cmd = "lazygit",
    dimensions = {height = 1, width = 1},
    border='rounded',
    hl = 'LazygitBackground'
  }):toggle()
end

return M
