local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  event = "BufRead",
  config = function()
    require("treesitter.config")
  end,
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "windwp/nvim-ts-autotag",
    "hiphish/rainbow-delimiters.nvim",
    {
      "m-demare/hlargs.nvim",
      config = function()
        require("hlargs").setup({
          highlight = { link = "Identifier" },
        })
      end,
    },
  },
})
