require("null-ls")
require("crates").setup({
  popup = {
    autofocus = true,
    border = "single",
  },
  null_ls = {
    enabled = true,
    name = "crates.nvim",
  },
})
