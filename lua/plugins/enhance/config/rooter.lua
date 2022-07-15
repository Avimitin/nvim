local opts = {
  rooter_patterns = { ".git", ".hg", ".svn", "Cargo.toml", "go.mod", "package.json" },
  -- Trigger manually
  manual = false,
}

require("nvim-rooter").setup(opts)

-- trigger it once
require("nvim-rooter").rooter()

vim.notify("Dir Change: " .. vim.fn.getcwd())
