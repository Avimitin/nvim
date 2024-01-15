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
    {
      "m-demare/hlargs.nvim",
      config = function()
        require("hlargs").setup({
          highlight = { link = "Identifier" },
          disable = function(_, buf)
            return vim.api.nvim_buf_line_count(buf) >= 5000
          end,
        })
      end,
    },
  },
})
