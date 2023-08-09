return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  event = "BufRead",
  config = function()
    require("plugins.treesitter.config")
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
}
