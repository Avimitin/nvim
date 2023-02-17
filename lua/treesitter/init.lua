local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  build = ":TSUpdate",
  event = "BufRead",
  config = function()
    require("treesitter.config")
  end,
})
