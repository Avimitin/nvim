local disable = function(lang, buf)
  local max_filesize = 1024 * 1024 -- 1MB
  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
  if ok and stats and stats.size > max_filesize then
    return true
  end
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
