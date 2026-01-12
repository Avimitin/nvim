local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  branch = "main",
  config = function()
    vim.schedule(function()
      require("treesitter.config")
    end)
  end,
})

register("nvim-treesitter/nvim-treesitter-textobjects", { branch = "main" })
register("windwp/nvim-ts-autotag", {})
