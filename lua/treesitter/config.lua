require("nvim-treesitter.configs").setup({
  -- packer compile is compiled without runtime context, so here we must give it
  -- the full path to the treesitter ft function for evaluating the filetype
  ensure_installed = vim.cfg.treesitter.ensure_installed,
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  matchup = {
    enable = true,
  },
  autotag = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
        ["ab"] = "@block.outer",
        ["ib"] = "@block.inner",
        ["al"] = "@call.outer",
        ["il"] = "@call.inner",
        ["aP"] = "@parameter.outer",
        ["iP"] = "@parameter.inner",
        ["ao"] = "@conditional.outer",
        ["io"] = "@conditional.inner",
        ["as"] = "@statement.outer",
      },
    },
  },
})

vim.api.nvim_command("set foldmethod=expr")
vim.api.nvim_command("set foldexpr=nvim_treesitter#foldexpr()")
