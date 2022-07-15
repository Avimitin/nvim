local au = vim.api.nvim_create_autocmd

-- rooter config are located in plugins/enhance/config/rooter.lua file
local rooter_opts = require("plugins.enhance.config.rooter")

-- trigger rooter once
au("VimEnter", {
  callback = function()
    require("nvim-rooter").setup(rooter_opts)
  end,
})
