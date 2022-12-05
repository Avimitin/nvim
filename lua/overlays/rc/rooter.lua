-- nvim-rooter is not present when user first install the configuration
local has_rooter, rooter = pcall(require, "nvim-rooter")
if not has_rooter then
  return
end

local opts = {
  rooter_patterns = { ".git", ".hg", ".svn", "Cargo.toml", "go.mod", "package.json" },
  exclude_filetypes = { "gitcommit" },
  -- Trigger manually
  manual = false,
}

local old_cwd = vim.loop.cwd()

rooter.setup(opts)
-- trigger it only once
rooter.rooter()

local new_cwd = vim.loop.cwd()

if new_cwd ~= old_cwd then
  vim.notify("Dir changed: " .. new_cwd)
end
