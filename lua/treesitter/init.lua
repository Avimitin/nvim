local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  event = "BufReadPost",
  config = function()
    vim.schedule(function()
      require("treesitter.config")
    end)
  end,
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "windwp/nvim-ts-autotag",
  },
})
