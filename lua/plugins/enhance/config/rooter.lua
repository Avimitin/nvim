-- nvim-rooter is not present when user first install the configuration
local has_rooter, rooter = pcall(require, "nvim-rooter")
if not has_rooter then
  return
end

local opts = {
  rooter_patterns = { ".git", ".hg", ".svn", "Cargo.toml", "go.mod", "package.json" },
  -- Trigger manually
  manual = false,
}

local uv = vim.loop

local old_cwd = uv.cwd()

rooter.setup(opts)
-- trigger it once
rooter.rooter()

local new_cwd = uv.cwd()

if new_cwd ~= old_cwd then
  vim.notify("Dir Change: " .. new_cwd)
end
