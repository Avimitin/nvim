require('packer').init {
  display = {
    open_fn = function()
      return require("packer.util").float {
        border = "single"
      }
    end
  },
  git = {
    clone_timeout = 60 -- Timeout, in seconds, for git clones
  }
}

local map = require('utils').map

return require('packer').startup(function(use)
  -- Packer can manage itself
  use {
    "wbthomason/packer.nvim",
    event = "VimEnter"
  }

  local component = {'colors', 'cmp', 'coding', 'mkd', 'git_tools', 'enhance'}
  for _, compo in ipairs(component) do
    for _, plugin in ipairs(require("partial."..compo)) do
      use(plugin)
    end
  end
end)

-- vim: foldmethod=marker
