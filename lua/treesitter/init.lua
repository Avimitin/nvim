local register = require("pack").register

register("nvim-treesitter/nvim-treesitter", {
  build = ":TSUpdate",
  event = "BufRead",
  config = function()
    require("treesitter.config")
  end,
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "windwp/nvim-ts-autotag",
    {
      "m-demare/hlargs.nvim",
      config = function()
        require("hlargs").setup({
          highlight = { link = "Identifier" },
        })
      end,
    },
    {
      "RRethy/vim-illuminate",
      config = function()
        require("illuminate").configure({
          -- no more regex
          providers = { "treesitter", "lsp" },
        })
      end,
    },
  },
})
