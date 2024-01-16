local disable = function(_, buf)
  return vim.api.nvim_buf_line_count(buf) >= 5000
end

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
    additional_vim_regex_highlighting = { "org" },
    disable = disable,
  },
  indent = {
    enable = true,
    disable = disable,
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
