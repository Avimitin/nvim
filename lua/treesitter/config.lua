require("nvim-treesitter.configs").setup({
  auto_install = false,
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = false,
      node_incremental = false,
      scope_incremental = false,
      node_decremental = false,
    },
  },
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

      keymaps = {},
    },
  },
})

vim.api.nvim_command("set foldmethod=expr")
vim.api.nvim_command("set foldexpr=nvim_treesitter#foldexpr()")
