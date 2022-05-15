require("nvim-treesitter.configs").setup({
  ensure_installed = vim.g.enable_treesitter_ft,
  highlight = {
    enable = true,
  },
  matchup = {
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
        ["ap"] = "@parameter.outer",
        ["ip"] = "@parameter.inner",
      },
    },
  },
})

-- extends those text objects for wildfire, and now we can simply press <ENTER> to select
-- inside function/class/blck/function call/parameters
vim.g.wildfire_objects = vim.list_extend(vim.g.wildfire_objects, { "if", "ic", "ib", "il", "ip" })
