-- use fterm to open the lazygit
local M = {}
local term = require("FTerm.terminal")
M.toggle = function()
  term:new():setup({
    cmd = "lazygit",
    dimensions = {height = 0.96, width = 0.98}
})
end

return M
