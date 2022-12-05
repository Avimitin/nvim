local packer = require("packer")

local all_repos = {}

local overlays = {
  "coding",
  "markdown",
  "theme",
  "enhance",
  "completion",
}

for _, ova in ipairs(overlays) do
  local repos = require("overlays." .. ova)
  vim.list_extend(all_repos, repos)
end

packer.startup(function(use)
  -- Packer can manage itself
  use({
    "wbthomason/packer.nvim",
    event = "VimEnter",
  })

  for _, repo in ipairs(all_repos) do
    use(repo)
  end
end)
