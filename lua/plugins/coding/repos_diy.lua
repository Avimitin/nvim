local config = require("plugins.coding.config")
config.pre()

local M = {
  {
    "neoclide/coc.nvim",
    branch = "release",
  } 
}

return M
