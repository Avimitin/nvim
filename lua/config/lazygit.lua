-- use fterm to open the lazygit
local M = {}
local term = require("FTerm.terminal")
M.toggle = function()
  term:new():setup({
    cmd = "lazygit",
    dimensions = {height = 0.92, width = 0.95},
    border='shadow',
    hl = 'LazygitBackground'
  }):toggle()
end

return M
