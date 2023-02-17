local register = require("pack").register

register("rebelot/kanagawa.nvim", {
  cond = vim.cfg.ui.theme == "kanagawa",
  config = function()
    require("ui.kanagawa")
  end,
})
