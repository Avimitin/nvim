local pack = require("pack").register

pack("lewis6991/gitsigns.nvim", {
  config = function()
    require("git.gitsigns")
  end,
})

pack("tpope/vim-fugitive", {
  cmd = "Git",
})
