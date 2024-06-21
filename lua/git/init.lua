local pack = require("pack").register

pack("lewis6991/gitsigns.nvim", {
  commit = "0dc886637f9686b7cfd245a4726f93abeab19d4a",
  config = function()
    require("git.gitsigns")
  end,
})
