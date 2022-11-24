local nvm_node = require('plugins.libs.nvm-node')
local config = require("plugins.coding.config")
config.pre()

local M = {
  {
    "neoclide/coc.nvim",
    branch = "release",
    run = function ()
      -- see https://github.com/neoclide/coc.nvim/issues/856
      nvm_node.compile_nvm_node_path()
    end,
    setup = function ()
      local node_bin_path = nvm_node.get_nvm_node_path()
      if not node_bin_path then
        return
      end
      vim.g.coc_node_path = node_bin_path
    end
  } 
}

return M
